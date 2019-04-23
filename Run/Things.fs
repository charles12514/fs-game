module Game.Things

open Engine
open Engine.Animation
open Util
open Util.Geometry

type Player (playerControl) as this =
    inherit MovableThing (10.0, Solid)

    let spritesheet = Convert.spritesheetToImages Resources.Spritesheet.Player
    do
        this.Actions <-
            [ Actions.updatePlayerVelocity playerControl this
              Actions.tryMove this ]
    
    interface IAnimatable with
        member this.Layer = Foreground
        member val SpriteOffset = Vec -this.Radius -this.Radius with get, set
        member this.GetSprite () =
            let (index, time) =
                let x = this.Rotation.x
                let y = this.Rotation.y
                match this.Motion with
                | Still t ->
                    let index =
                        if eq0 x && gt0 y then SpriteIndex.Still.Up
                        elif lt0 x && gt0 y then SpriteIndex.Still.UpLeft
                        elif lt0 x && eq0 y then SpriteIndex.Still.Left
                        elif lt0 x && lt0 y then SpriteIndex.Still.DownLeft
                        elif eq0 x && lt0 y then SpriteIndex.Still.Down
                        elif gt0 x && lt0 y then SpriteIndex.Still.DownRight
                        elif gt0 x && eq0 y then SpriteIndex.Still.Right
                        else SpriteIndex.Still.UpRight
                    (index, t)
                | Moving t ->
                    let index =
                        if eq0 x && gt0 y then SpriteIndex.Moving.Up
                        elif lt0 x && gt0 y then SpriteIndex.Moving.UpLeft
                        elif lt0 x && eq0 y then SpriteIndex.Moving.Left
                        elif lt0 x && lt0 y then SpriteIndex.Moving.DownLeft
                        elif eq0 x && lt0 y then SpriteIndex.Moving.Down
                        elif gt0 x && lt0 y then SpriteIndex.Moving.DownRight
                        elif gt0 x && eq0 y then SpriteIndex.Moving.Right
                        else SpriteIndex.Moving.UpRight
                    (index, t)
            let loopIndex = getLoopIndex (Array.length spritesheet.[index]) 100 time
            spritesheet.[index].[loopIndex]

let rock0Radius = 20.0
type Rock () =
    inherit StaticThing (Collider.makeCircle Vector.Zero rock0Radius, Solid)

    let spritesheet = Convert.spritesheetToImages Resources.Spritesheet.Rock0

    interface IAnimatable with
        member this.Layer = Foreground
        member val SpriteOffset = Vec -rock0Radius -rock0Radius with get, set
        member this.GetSprite () = spritesheet.[0].[0]
    
type GhostRock () =
    inherit StaticThing (Collider.makeCircle Vector.Zero rock0Radius, Intangible)

    let spritesheet = Convert.spritesheetToImages Resources.Spritesheet.Rock0

    interface IAnimatable with
        member this.Layer = Foreground
        member val SpriteOffset = Vec -rock0Radius -rock0Radius with get, set
        member this.GetSprite () = spritesheet.[0].[0]
    
type SkyRock () =
    inherit StaticThing (Collider.makeCircle Vector.Zero rock0Radius, Intangible)

    let spritesheet = Convert.spritesheetToImages Resources.Spritesheet.Rock0

    interface IAnimatable with
        member this.Layer = Sky
        member val SpriteOffset = Vec -rock0Radius -rock0Radius with get, set
        member this.GetSprite () = spritesheet.[0].[0]
    
type BackgroundRock () =
    inherit StaticThing (Collider.makeCircle Vector.Zero rock0Radius, Intangible)

    let spritesheet = Convert.spritesheetToImages Resources.Spritesheet.Rock0

    interface IAnimatable with
        member this.Layer = Background
        member val SpriteOffset = Vec -rock0Radius -rock0Radius with get, set
        member this.GetSprite () = spritesheet.[0].[0]

let teleporterRadius = 10.0
type Teleporter (destination) as this =
    inherit StaticThing (Collider.makeCircle Vector.Zero teleporterRadius, Intangible)

    let spritesheet = Convert.spritesheetToImages Resources.Spritesheet.Teleporter
    let mutable time = System.TimeSpan.Zero
    do
        this.Actions <-
            [ Actions.teleport this destination
              SelfUpdate (fun elapsedTime -> time <- time + elapsedTime) ]

    interface IAnimatable with
        member this.Layer = Foreground
        member val SpriteOffset = Vec -teleporterRadius -teleporterRadius with get, set
        member this.GetSprite () =
            spritesheet.[0].[getLoopIndex (Array.length spritesheet.[0]) 100 time]

let fenceCollider =
    let width = 62.0
    let height = 5.0
    Collider.makePolygon
        (Polygon.make
            [ Vec 0.0 0.0
              Vec width 0.0
              Vec width height
              Vec 0.0 height ])
type FenceH () =
    inherit StaticThing (fenceCollider, Solid)

    let spritesheet = Convert.spritesheetToImages Resources.Spritesheet.Fence
    
    interface IAnimatable with
        member this.Layer = Background
        member val SpriteOffset = Vector.Zero with get, set
        member this.GetSprite () = spritesheet.[0].[0]
