module Game.Resources

open System.Drawing

let private resourceManager =
    new System.Resources.ResourceManager ("Resources", System.Reflection.Assembly.GetExecutingAssembly ())

let private getBitmap name =
    resourceManager.GetObject name
    :?> Bitmap

let private splitSpriteRow width height startHeight (bitmap:Bitmap) =
    let possibleFrames = bitmap.Width / width
    [| 0 .. possibleFrames - 1 |]
    |> Array.choose
        (fun i ->
            let rect = Rectangle (i * width, startHeight, width, height)
            let frame = bitmap.Clone (rect, bitmap.PixelFormat)
            let blank = ref true
            for x in 0 .. width - 1 do
                for y in 0 .. height - 1 do
                    if !blank then
                        let pixel = frame.GetPixel (x, y)
                        blank := pixel.A > 0uy |> not
            if not !blank then Some frame
            else None)

let private splitSpritesheet width height (bitmap:Bitmap) =
    let rows = bitmap.Height / height
    [| 0 .. rows - 1 |]
    |> Array.map (fun i -> splitSpriteRow width height (i * height) bitmap)

module Spritesheet =
    let private load name width height =
        name |> getBitmap |> splitSpritesheet width height
    let Player = load "Spritesheet.Player" 21 60
    let Rock0 = load "Spritesheet.Rock0" 40 70
    let Placeholder = load "Spritesheet.Placeholder" 10 10
    let Fence = load "Spritesheet.Fence" 62 60
    let Teleporter = load "Spritesheet.Teleporter" 21 30
