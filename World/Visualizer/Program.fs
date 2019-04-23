open Biome
open PlateTectonics
open System
open System.Drawing
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes
open Util
open Util.Constants
open Util.Geometry
open System.IO
open System.Drawing.Imaging

[<EntryPoint; STAThread>]
let main _ =
    let scale = 10

    let seed = 0
    let height = 80
    let width = 80

    // Plate Tectonics
    let plateCount = 26
    let pNext = 0.3
    let pctLand = 0.4
    let pIsland = 0.05
    let voidChunkSize = 2

    // Wind
    let maxVoid = 2
    let windCenters = 5

    // Temperature
    let minTemp = -10.0
    let maxTemp = 30.0
    let tempSimplexLayers = 3
    let tempSimplexScale = 1.0 / 200.0
    let tempSimplexOffset = 10.0
    let foothillTempDecrease = -5.0
    let oceanCurrent = 0.2

    // Precipitation
    let rain = 0.1
    let maxRainPerSquare = 20.0
    let minRain = 0.0
    let maxRain = 420.0

    let rng = Random seed
    let map = PlateTectonics.makeLand rng height width plateCount pNext pctLand pIsland voidChunkSize maxVoid
    let wind = Wind.generateWind rng height width windCenters
    let temperature =
        Temperature.withLand (rng.NextDouble () * 10.0) tempSimplexLayers tempSimplexScale height width tempSimplexOffset foothillTempDecrease oceanCurrent minTemp maxTemp map
    let rain =
        Precipitation.calculate rain maxRainPerSquare minRain maxRain map temperature wind
    let biome =
        Biome.classify map temperature rain
    
    let window = Window ()
    window.Title <- "Map"
    window.SizeToContent <- SizeToContent.WidthAndHeight

    let canvas = Canvas ()
    canvas.Background <- Brushes.Beige
    canvas.Height <- Array2D.length1 wind * scale |> float
    canvas.Width <- Array2D.length2 wind * scale |> float
    window.Content <- canvas

    use bitmap = new System.Drawing.Bitmap (width * scale, height * scale)

    let drawPixel color p =
        bitmap.SetPixel (int p.x, int p.y, color)
    let drawLine color (p0:Vector) p1 =
        let d = p1 - p0
        for i in 0 .. scale do
            let p = p0 + (d * float i / float scale)
            drawPixel color p
    let drawRectangle color x y =
        for i in x .. x + scale - 1 do
            for j in y .. y + scale - 1 do
                bitmap.SetPixel (i, j, color)

    let drawLand i j =
        let color = Color.Black
        let drawPixel = drawPixel color
        let drawLine = drawLine color
        match map.[i, j] with
        | Void -> ()
        | Ocean ->
            let p0 =
                Vec (float j + 0.2) (float i + 0.6)
                |> (*) scale
            let p1 = p0 + (Vec 0.3 0.0 * scale)
            let p2 = p1 + (Vec 0.3 0.0 * scale)
            drawPixel p0
            drawPixel p1
            drawPixel p2
        | Land ->
            let p0 =
                Vec (float j + 0.2) (float i + 0.6)
                |> (*) scale
            let p1 = p0 + (Vec 0.6 0.0 * scale)
            drawLine p0 p1
        | Foothill ->
            let p0 =
                Vec (float j + 0.1) (float i + 0.9)
                |> (*) scale
            let p1 = p0 + (Vec 0.4 -0.4 * scale)
            let p2 = p0 + (Vec 0.8 0.0 * scale)
            drawLine p0 p1
            drawLine p1 p2
        | Mountain ->
            let p0 =
                Vec (float j + 0.1) (float i + 0.9)
                |> (*) scale
            let p1 = p0 + (Vec 0.4 -0.8 * scale)
            let p2 = p0 + (Vec 0.8 0.0 * scale)
            drawLine p0 p1
            drawLine p1 p2

    let drawWind i j =
        let drawLine = drawLine Color.LightSkyBlue
        let p1 =
            if Vector.mag2 wind.[i, j] |> eq0 then Vector.Zero
            else
                Vec wind.[i, j].x -wind.[i, j].y |> Vector.ofLength (0.6 * float scale)
        let p0 = p1 |> Vector.rotate (PI * -2.0 / 3.0)
        let p2 = p1 |> Vector.rotate (PI * 2.0 / 3.0)
        let m = Vec (j * scale + scale |> float) (i * scale + scale |> float)
        let p0 = p0 + m
        let p1 = p1 + m
        let p2 = p2 + m
        drawLine p0 p1
        drawLine p1 p2

    let drawTemp i j =
        let temp = (temperature.[i, j] - minTemp) / (maxTemp - minTemp) |> (-) 1.0 |> (*) 255.0 |> int
        drawRectangle (Drawing.Color.FromArgb (255, 255, temp, temp)) (j * scale) (i * scale)

    let drawRain i j =
        let r = (rain.[i, j] - minRain) / (maxRain - minRain) |> (-) 1.0 |> (*) 255.0 |> int
        drawRectangle (Drawing.Color.FromArgb (255, r, r, 255)) (j * scale) (i * scale)

    let drawBiome i j =
        let color =
            match biome.[i, j] with
            | Biome.Ocean -> Color.Aqua
            | Biome.Void -> Color.Gray
            | Tundra -> Color.White
            | SubtropicalDesert -> Color.SandyBrown
            | TropicalRainforest -> Color.DarkGreen
            | TropicalSeasonalForestSavanna -> Color.Brown
            | TemperateRainforest -> Color.ForestGreen
            | TemperateSeasonalForest -> Color.Green
            | WoodlandShrubland -> Color.GreenYellow
            | TemperateGrasslandColdDesert -> Color.LightGreen
            | BorealForest -> Color.LightBlue
        drawRectangle color (j * scale) (i * scale)

    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
//            drawTemp i j
//            drawRain i j
            drawBiome i j
            drawLand i j
//            if i % 2 = 1 && j % 2 = 1 then
//                drawWind (i - 1) (j - 1)

    ignore (drawLand, drawWind, drawTemp, drawRain, drawBiome)

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

    bitmap |> bitmapToImage |> canvas.Children.Add |> ignore

    let application = Application ()
    application.Run window
