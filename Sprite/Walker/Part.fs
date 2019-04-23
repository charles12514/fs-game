module Part

open System.Drawing
open System.Windows.Media
open Util.Geometry

[<NoComparison>]
type Img =
  { Bitmap : Bitmap option
    AngleIn : Vector
    PointIn : Vector
    PointOut : Vector }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Img =
    let dir img = img.PointOut - img.PointIn

[<NoComparison; NoEquality>]
type Component =
    | Joint of Img * (float -> float)
    | Connector of Img

[<NoComparison; NoEquality>]
type Layer =
    | Fore of Form
    | Aft of Form

and [<NoComparison; NoEquality>] Form =
    | Nexus of Layer * Layer
    | Component of Component * Layer
    | Extremity of Img

[<NoComparison>]
type Placement =
  { Bitmap : Bitmap
    Layer : int
    Transform : Transform }

let place =
    let tryCreatePlacement point dir layer (img:Img) =
        img.Bitmap
        |> Option.map
            (fun bmp ->
                { Layer = layer
                  Bitmap = bmp
                  Transform =
                    let v = img.AngleIn |> Vector.angleTo dir
                    let p = point - img.PointIn
                    let t = TransformGroup ()
                    RotateTransform (v * -180.0 / Util.Constants.PI, img.PointIn.x, img.PointIn.y) |> t.Children.Add
                    TranslateTransform (-p.x, p.y) |> t.Children.Add
                    t })
    let rec place t startPoint startDir layer placed form =
        match form with
        | Nexus (a, b) ->
            match (a, b) with
            | (Fore f, Fore a)
            | (Aft f, Aft a)
            | (Fore f, Aft a)
            | (Aft a, Fore f) ->
                let (layer, placed) = place t startPoint startDir layer placed f
                place t startPoint startDir layer placed a
        | Component (comp, Fore next) ->
            let (point, dir) =
                match comp with
                | Joint (_, activation) -> (startPoint, Vector.rotate (activation t) startDir)
                | Connector img -> (startPoint + Vector.ofLength (img |> Img.dir |> Vector.mag) startDir, startDir)
            let (layer, placed) = place t point dir layer placed next
            let img =
                match comp with
                | Joint (img, _) | Connector img -> img
            let placed =
                tryCreatePlacement startPoint startDir layer img
                |> Option.fold
                    (fun placed p -> p :: placed)
                    placed
            (layer + 1, placed)
        | Component (comp, Aft next) ->
            let img =
                match comp with
                | Joint (img, _) | Connector img -> img
            let placed =
                tryCreatePlacement startPoint startDir layer img
                |> Option.fold
                    (fun placed p -> p :: placed)
                    placed
            let (point, dir) =
                match comp with
                | Joint (_, activation) ->
                    (startPoint, Vector.rotate (activation t) startDir)
                | Connector img -> (startPoint + Vector.ofLength (img |> Img.dir |> Vector.mag) startDir, startDir)
            place t point dir (layer + 1) placed next
        | Extremity img ->
            let placed =
                tryCreatePlacement startPoint startDir layer img
                |> Option.fold
                    (fun placed p -> p :: placed)
                    placed
            (layer + 1, placed)
    fun t point dir ->
        place t point dir 0 []
        >> snd
