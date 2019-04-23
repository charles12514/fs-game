namespace Engine

open System
open System.Windows.Controls
open Util.Geometry

[<NoComparison; NoEquality>]
type Action<'a> =
    | SelfUpdate of (TimeSpan -> unit)
    | SceneUpdate of ('a -> TimeSpan -> unit)
    | GlobalUpdate of ('a -> TimeSpan -> unit)

type Motion =
    | Still of TimeSpan
    | Moving of TimeSpan
    
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Motion =
    let fold oldMotion newMotion =
        match (oldMotion, newMotion) with
        | (Still t1, Still t2) -> Still (t1 + t2)
        | (Moving t1, Moving t2) -> Moving (t1 + t2)
        | (_, newMotion) -> newMotion
        
type Thing () =
    member val Parent : Thing option = None with get, set
    member val Children : Thing list = [] with get, set
    member val Actions : Action<Thing> list = [] with get, set
    member val RelativePosition = Vector.Zero with get, set
    member this.AbsolutePosition
        with get () =
            match this.Parent with
            | None -> this.RelativePosition
            | Some parent -> parent.AbsolutePosition + this.RelativePosition
        and set value =
            this.RelativePosition <-
                match this.Parent with
                | None -> value
                | Some parent -> value - parent.AbsolutePosition

type Scene () =
    inherit Thing ()

type MovableThing (radius, collisionClass) =
    inherit Thing ()

    member val Radius = radius with get, set
    member val Velocity = Vector.Zero with get, set
    member val Rotation = Vector.Zero with get, set
    member val Motion = Still TimeSpan.Zero with get, set

    interface ICollidable with
        member val CollisionClass = collisionClass with get, set
        member this.Collider = Collider.makeCircle this.AbsolutePosition this.Radius

type StaticThing (collider, collisionClass) =
    inherit Thing ()

    interface ICollidable with
        member val CollisionClass = collisionClass with get, set
        member this.Collider = Collider.transform ((+) this.AbsolutePosition) collider

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Thing =
    let removeChild child (thing:Thing) =
        if thing.Children |> List.exists ((=) child) then
            child.Parent <- None
            thing.Children <-
                thing.Children
                |> List.filter ((<>) child)
    let setParent parent (thing:Thing) =
        thing.Parent |> Option.iter (removeChild thing)
        thing.Parent <- parent
        parent |> Option.iter (fun p -> p.Children <- thing :: p.Children)
    let rec rootParent (thing:Thing) =
        match thing.Parent with
        | None -> thing
        | Some parent -> rootParent parent
    let rec containingScene (thing:Thing) =
        match thing with
        | :? Scene -> Some thing
        | _ ->
            match thing.Parent with
            | None -> None
            | Some parent -> containingScene parent
    let childrenWhere f thing =
        let rec getChildren (thing:Thing) =
            thing.Children
            |> List.filter f
            |> List.collect (fun child -> child :: getChildren child)
        getChildren thing
        