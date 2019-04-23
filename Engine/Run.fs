module Game

open Engine
open Engine.Animation
open System
open System.Collections.Generic
open System.Linq
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Util.Geometry

let makeDrawer (canvas:Canvas) camera =
    let graphic = Dictionary<IAnimatable, Image> ()
    let setPositionOnCanvas (obj:IAnimatable) zIndex =
        let element = graphic.[obj]
        let position = (obj :?> Thing).AbsolutePosition + obj.SpriteOffset
        Canvas.SetLeft (element, position.x)
        Canvas.SetBottom (element, position.y)
        Canvas.SetZIndex (element, zIndex)
    fun () ->
        let window = canvas.Parent :?> Window
        canvas.Height <- window.Height
        canvas.Width <- window.Width
        let scene = Thing.containingScene camera
        let inScene =
            match scene with
            | Some scene ->
                scene
                |> Thing.childrenWhere
                    (function
                     | :? Scene -> false
                     | _  -> true)
                |> List.choose
                    (fun child ->
                        match box child with
                        | :? IAnimatable as animatable -> Some animatable
                        | _ -> None)
            | None -> []
        graphic.Keys
        |> Seq.toList
        |> List.iter
            (fun thing ->
                if inScene |> List.contains thing |> not then
                    canvas.Children.Remove graphic.[thing]
                    graphic.Remove thing |> ignore)

        let transform = canvas.RenderTransform :?> TransformGroup
        let translate = transform.Children.[0] :?> TranslateTransform
        let canvasCenter = Vec canvas.Width canvas.Height * 0.5
        let t = camera.AbsolutePosition - canvasCenter
        translate.X <- -t.x
        translate.Y <- t.y
        let scale = transform.Children.[1] :?> ScaleTransform
        scale.CenterX <- canvasCenter.x
        scale.CenterY <- canvasCenter.y

        inScene
        |> List.sortBy
            (fun obj ->
                let position = (obj :?> Thing).AbsolutePosition + obj.SpriteOffset
                (obj.Layer, -position.y, position.x))
        |> List.iteri
            (fun index obj ->
                let frame = obj.GetSprite ()
                match graphic.TryGetValue obj with
                | (true, f) when f <> frame -> f.Visibility <- Visibility.Collapsed
                | _ -> ()
                frame.Visibility <- Visibility.Visible
                if canvas.Children.Contains frame |> not then
                    canvas.Children.Add frame |> ignore
                graphic.[obj] <- frame
                setPositionOnCanvas obj index)

let makeActer state =
    let mutable lastTime = DateTime.UtcNow
    fun () ->
        let now = DateTime.UtcNow
        let elapsed = now - lastTime
        State.step elapsed state
        lastTime <- now

let run name (state:State) camera =
    let window = Window ()
    window.Title <- name
    //window.SizeToContent <- SizeToContent.WidthAndHeight
    window.WindowState <- WindowState.Maximized
    window.WindowStyle <- WindowStyle.None

    let canvas = Canvas ()
    canvas.Background <- Brushes.DimGray
    window.Content <- canvas

    let transform = TransformGroup ()
    TranslateTransform () |> transform.Children.Add
    ScaleTransform (2.0, 2.0) |> transform.Children.Add
    canvas.RenderTransform <- transform

    window.Closing.Add (fun _ -> state.Quit <- true)
    window.KeyDown.Add (fun event -> state.SetKeyDown event.Key)
    window.KeyUp.Add (fun event -> state.SetKeyUp event.Key)

    let act = makeActer state
    let draw = makeDrawer canvas camera

    CompositionTarget.Rendering.Add
        (fun _ ->
            if state.Quit then
                window.Close ()
            else
                act ()
                draw ())

    let application = Application ()
    application.Run window |> ignore
