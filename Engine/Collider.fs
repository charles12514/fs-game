namespace Engine

open Util
open Util.Geometry

type PolygonCollider =
  { Bounds : Polygon
    BoundingCircle : Circle }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PolygonCollider =
    let make polygon =
        let points = Polygon.points polygon
        let (left, right) =
            let x = points |> List.map Vector.getX
            (List.min x, List.max x)
        let (bottom, top) =
            let y = points |> List.map Vector.getY
            (List.min y, List.max y)
        let center = Vec ((left + right) * 0.5) ((bottom + top) * 0.5)
        let radius = Vec right top |> Vector.distanceTo center
        { Bounds = polygon
          BoundingCircle = { Center = center; Radius = radius } }

type Collider =
    | Polygon of PolygonCollider
    | Circle of Circle

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Collider =
    let makeCircle center radius =
        Circle { Center = center; Radius = radius }
    let makePolygon polygon =
        Polygon (PolygonCollider.make polygon)

    let boundingCircle =
        function
        | Circle bound | Polygon { BoundingCircle = bound } -> bound

    let transform f =
        function
        | Circle circle ->
            makeCircle (f circle.Center) circle.Radius
        | Polygon polygonCollider ->
            makePolygon (Polygon.map f polygonCollider.Bounds)

    let private bumpCircle source velocity target =
        let perpendicular = target.Center - source.Center |> Vector.perpendicular
        velocity |> Vector.project perpendicular

    let private bumpSegment velocity segment =
        velocity |> Vector.project segment.Dir

    let private bumpPoint circle velocity point =
        let perpendicular = point - circle.Center |> Vector.perpendicular
        velocity |> Vector.project perpendicular

    let private tryBumpPolygon movedCircle velocity polygon =
        let r2 = movedCircle.Radius * movedCircle.Radius
        let segmentBumps =
            polygon
            |> Polygon.segments
            |> List.choose
                (fun seg ->
                    if seg.Dir |> Vector.cross (movedCircle.Center - seg.Start) |> lt0
                       && seg.Dir |> Vector.dot (movedCircle.Center - seg.Start) |> geq0
                       && -seg.Dir |> Vector.dot (movedCircle.Center - seg.End) |> geq0
                       && Segment.sqDistanceToPoint movedCircle.Center seg <= r2
                       && seg.Dir |> Vector.perpendicular |> Vector.dot velocity |> gt0 then
                        bumpSegment velocity seg |> Some
                    else None)
        let pointBumps =
            polygon
            |> Polygon.pointTriples
            |> Seq.choose
                (fun (a, b, c) ->
                    if b |> Vector.sqDistanceTo movedCircle.Center <= r2
                       && b - a |> Vector.dot (movedCircle.Center - b) |> gt0
                       && b - c |> Vector.dot (movedCircle.Center - b) |> gt0
                       && b - movedCircle.Center |> Vector.dot velocity |> gt0 then
                        bumpPoint movedCircle velocity b |> Some
                    else None)
            |> Seq.toList
        segmentBumps @ pointBumps
        |> function
            | [] -> None
            | v :: [] -> Some v
            | (head :: tail) as list ->
                if List.forall (fun t -> Vector.cross head t |> eq0 && Vector.dot head t |> gt0) tail then
                    list |> List.minBy Vector.mag2 |> Some
                else Some Vector.Zero

    // TODO fix same circles returning NaN
    let tryBump source velocity target =
        match target with
        | Circle target ->
            if Circle.intersectsCircle { source with Center = source.Center + velocity } target then
                bumpCircle source velocity target |> Some
            else None
        | Polygon target ->
            let endCircle = { source with Center = source.Center + velocity }
            if Circle.intersectsCircle endCircle target.BoundingCircle then
                tryBumpPolygon endCircle velocity target.Bounds
            else None

    let intersects a b =
        if Circle.intersectsCircle (boundingCircle a) (boundingCircle b) then
            match (a, b) with
            | (Circle _, Circle _) -> true
            | (Circle a, Polygon b) | (Polygon b, Circle a) -> Circle.intersectsPolygon b.Bounds a
            | (Polygon a, Polygon b) -> Polygon.intersectsPolygon a.Bounds b.Bounds
        else false
        
type CollisionClass =
    | Solid
    | Intangible

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module CollisionClass =
    let canIntersect source target =
        source = Intangible || target = Intangible

type ICollidable =
    abstract member CollisionClass : CollisionClass with get, set
    abstract member Collider : Collider
