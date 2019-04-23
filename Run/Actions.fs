module Game.Actions

open Engine
open Game.Input
open System
open Util.Geometry

let updatePlayerVelocity (playerControl:PlayerControl) (thing:MovableThing) =
    let invSqrt2 = 1.0 / sqrt 2.0
    let playerSpeed = 100.0 // TODO this belongs somewhere else
    fun (state:Thing) (elapsedTime:TimeSpan) ->
        let state = state :?> State
        let v =
            let dx = (if state.IsKeyDown playerControl.Left then -1.0 else 0.0) + (if state.IsKeyDown playerControl.Right then 1.0 else 0.0)
            let dy = (if state.IsKeyDown playerControl.Down then -1.0 else 0.0) + (if state.IsKeyDown playerControl.Up then 1.0 else 0.0)
            let (dx, dy) =
                if dx <> 0.0 && dy <> 0.0 then (dx * invSqrt2, dy * invSqrt2)
                else (dx, dy)
            Vec dx dy
            |> (*) (playerSpeed * elapsedTime.TotalSeconds)
        thing.Velocity <- v
        if v <> Vector.Zero then
            thing.Rotation <- Vector.normalize v
    |> GlobalUpdate

let tryMove (thing:MovableThing) =
    let tryCollide onCollide (target:ICollidable) =
        if thing.Velocity <> Vector.Zero then
            let collidable = thing :> ICollidable
            if CollisionClass.canIntersect collidable.CollisionClass target.CollisionClass |> not then
                let sourceCollider =
                    match collidable.Collider with
                    | Circle sourceCollider -> Some sourceCollider
                    | Polygon _ -> None
                sourceCollider
                |> Option.iter
                    (fun sourceCollider ->
                        match Collider.tryBump sourceCollider thing.Velocity target.Collider with
                        | Some bumpedVelocity -> thing.Velocity <- onCollide bumpedVelocity
                        | None -> ())
    fun scene elapsedTime ->
        let targets =
            scene
            |> Thing.childrenWhere
                (box
                 >> function
                    | :? ICollidable as t -> t <> (thing :> ICollidable)
                    | _ -> false)
            |> List.map (fun child -> box child :?> ICollidable)
        targets
        |> List.iter (tryCollide id)
        targets
        |> List.iter (tryCollide (fun _ -> Vector.Zero))
        thing.RelativePosition <- thing.RelativePosition + thing.Velocity
        thing.Motion <-
            if thing.Velocity = Vector.Zero then Still elapsedTime
            else Moving elapsedTime
            |> Motion.fold thing.Motion
    |> SceneUpdate

let teleport (teleporter:ICollidable) (destination:Scene) =
    fun (scene:Thing) _ ->
        let teleporterCollider = teleporter.Collider
        scene.Children
        |> List.iter
            (fun child ->
                match box child with
                | :? ICollidable as collidable ->
                    if collidable <> teleporter && Collider.intersects teleporterCollider collidable.Collider then
                        Thing.setParent (destination :> Thing |> Some) child
                | _ -> ())
    |> SceneUpdate

let quit =
    fun (state:Thing) _ ->
        let state = state :?> State
        if state.IsKeyDown KeyBinding.Quit then
            state.Quit <- true
    |> GlobalUpdate
