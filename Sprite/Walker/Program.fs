open Part
open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open Util.Constants
open Util.Geometry

let mutable time = 0.0

let bitmapToBitmapImage (bitmap:Bitmap) =
    use stream = new MemoryStream ()
    bitmap.Save (stream, ImageFormat.Png)
    stream.Position <- 0L
    let bitmapImage = BitmapImage ()
    bitmapImage.BeginInit ()
    bitmapImage.StreamSource <- stream
    bitmapImage.CacheOption <- BitmapCacheOption.OnLoad
    bitmapImage.EndInit ()
    bitmapImage

let bitmapToImage bitmap =
    let bitmap = bitmapToBitmapImage bitmap
    let image = Image ()
    image.BeginInit ()
    image.Source <- bitmap
    image.Width <- float bitmap.PixelWidth
    image.Height <- float bitmap.PixelHeight
    image.EndInit ()
    image

let addToCanvas (canvas:Canvas) (obj:Part.Placement) =
    let image = bitmapToImage obj.Bitmap
    image.RenderTransform <- obj.Transform
    canvas.Children.Add image |> ignore
    Canvas.SetZIndex (image, -obj.Layer)

let place (canvas:Canvas) time point dir form =
    Part.place time point dir form
    |> List.iter (addToCanvas canvas)

let makeSin min max =
    sin >> (+) 1.0 >> (*) 0.5 >> (*) (max - min) >> (+) min
let inline phaseShift f = (~-) >> f
let rec phaseShiftForm =
    function
    | Extremity e -> Extremity e
    | Nexus (Fore a, Fore b) -> Nexus (phaseShiftForm a |> Fore, phaseShiftForm b |> Fore)
    | Nexus (Fore a, Aft b) -> Nexus (phaseShiftForm a |> Fore, phaseShiftForm b |> Aft)
    | Nexus (Aft a, Fore b) -> Nexus (phaseShiftForm a |> Aft, phaseShiftForm b |> Fore)
    | Nexus (Aft a, Aft b) -> Nexus (phaseShiftForm a |> Aft, phaseShiftForm b |> Aft)
    | Component (comp, layer) ->
        let comp =
            match comp with
            | Connector c -> Connector c
            | Joint (i, f) -> Joint (i, phaseShift f)
        let layer =
            match layer with
            | Fore f -> phaseShiftForm f |> Fore
            | Aft a -> phaseShiftForm a |> Aft
        Component (comp, layer)

let makeForm () =
    let path = @"C:\Users\csnyder\Desktop\Segment.png"
    let bmp = new Bitmap (path)
    let img =
      { Bitmap = Some bmp
        AngleIn = Vec 0.0 1.0
        PointIn = Vec 2.5 2.5
        PointOut = Vec 2.5 12.5 }
    let noImg =
      { Bitmap = None
        AngleIn = Vector.Zero
        PointIn = Vector.Zero
        PointOut = Vector.Zero }
    let foot = Extremity img
    let ankle = Joint (noImg, ((*) 2.0 >> makeSin (PI / 2.0 - PI / 8.0) (PI / 2.0 + PI / 16.0)))
    let shin = Connector img
    let knee = Joint (noImg, (makeSin (-PI / 6.0) 0.0))
    let thigh = Connector img
    let hip = Joint (noImg, (makeSin (-PI / 8.0) (PI / 8.0)))
    let leg =
        Component (hip, Fore (Component (thigh, Aft (Component (knee, Aft (Component (shin, Aft (Component (ankle, Aft foot)))))))))
    let leg' = phaseShiftForm leg
    let waist = Nexus (Fore leg, Aft leg')
    waist

[<EntryPoint; STAThread>]
let main _ =
    let window = Window ()
    window.Title <- "walk it off"
    window.SizeToContent <- SizeToContent.WidthAndHeight

    let canvas = Canvas ()
    canvas.Width <- 500.0
    canvas.Height <- 500.0
    canvas.Background <- Brushes.LightGray
    window.Content <- canvas

    window.KeyDown.Add
        (fun event ->
            let d = 0.1
            let d =
                if event.Key = Input.Key.A then -d
                elif event.Key = Input.Key.D then d
                else 0.0
            time <- time + d)

    canvas.RenderTransform <-
        let scale = 5.0
        let offset = 50.0
        let t = TransformGroup ()
        ScaleTransform (scale, scale) |> t.Children.Add
        TranslateTransform (offset, offset) |> t.Children.Add
        t

    let form = makeForm ()
    CompositionTarget.Rendering.Add
        (fun _ ->
            canvas.Children.Clear ()
            place canvas time Vector.Zero (Vec 0.0 1.0) form)

    let application = Application ()
    application.Run window
