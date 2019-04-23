module Engine.Animation

open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Windows.Controls
open System.Windows.Media.Imaging
open Util.Geometry

module SpriteIndex =
    module Still =
        let Up = 0
        let UpLeft = 1
        let Left = 2
        let DownLeft = 3
        let Down = 4
        let DownRight = 5
        let Right = 6
        let UpRight = 7
    module Moving =
        let Up = 8
        let UpLeft = 9
        let Left = 10
        let DownLeft = 11
        let Down = 12
        let DownRight = 13
        let Right = 14
        let UpRight = 15
        
type Layer =
    | Background
    | Ground
    | Foreground
    | Sky
    | UI of int

module Convert =
    let private bitmapToBitmapImage (bitmap:Bitmap) =
        use stream = new MemoryStream ()
        bitmap.Save (stream, ImageFormat.Png)
        stream.Position <- 0L
        let bitmapImage = BitmapImage ()
        bitmapImage.BeginInit ()
        bitmapImage.StreamSource <- stream
        bitmapImage.CacheOption <- BitmapCacheOption.OnLoad
        bitmapImage.EndInit ()
        bitmapImage

    let private bitmapToImage bitmap =
        let bitmap = bitmapToBitmapImage bitmap
        let image = Image ()
        image.BeginInit ()
        image.Source <- bitmap
        image.Width <- float bitmap.PixelWidth
        image.Height <- float bitmap.PixelHeight
        image.EndInit ()
        image

    let spritesheetToImages =
        Array.map (Array.map bitmapToImage)

type IAnimatable =
    abstract member Layer : Layer
    abstract member SpriteOffset : Vector with get, set
    abstract member GetSprite : unit -> Image
    
let getLoopIndex loopLength frameLength (time:System.TimeSpan) =
    if loopLength = 1 then 0
    else (int time.TotalMilliseconds / frameLength) % loopLength
