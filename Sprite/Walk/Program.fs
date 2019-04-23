open System
open System.Diagnostics
open System.Drawing
open System.IO
open System.Numerics
open Util.Geometry
open Walk

// conventions:
// x = +right, -left
// y = +up, -down
// z = +in, -out
// (0, 0, 0) is directly between character's feet
// character faces +z

module Rotation =
    let private sideOffset = 15.0
    let Up = 0.0
    let UpLeft = 45.0
    let Left = 90.0 + sideOffset
    let DownLeft = 135.0
    let Down = 180.0
    let DownRight = -135.0
    let Right = -90.0 - sideOffset
    let UpRight = -45.0
    
let Pi = Util.Constants.PI
let degreesToRadians = (*) (Pi / 180.0)

// yaw = y, pitch = x, roll = z
let viewPitch = degreesToRadians 10.0
let pitchBack = Quaternion.CreateFromYawPitchRoll (0.0f, float32 -viewPitch, 0.0f)
let pitchUp = Quaternion.CreateFromYawPitchRoll (0.0f, float32 viewPitch, 0.0f)
let viewPlane = Vector3.Transform (Vector3.UnitZ, pitchBack) |> Plane.make Vector3.Zero
let to2d =
    Plane.projectShapeOnto viewPlane
    >> Shape.transform pitchUp

let createTransparentBitmap (width:int) height =
    let bitmap = new Bitmap (width, height)
    for x in 0 .. width - 1 do
        for y in 0 .. height - 1 do
            bitmap.SetPixel (x, y, Color.Transparent)
    bitmap
    
let rec getLinePoints line =
    let round v = Vec (v.x |> Math.Floor |> (+) 0.5) (v.y |> Math.Floor |> (+) 0.5)
    let v1 = Vec (float line.Start.X) (float line.Start.Y)
    let v2 = Vec (float line.End.X) (float line.End.Y)
    let v1 = round v1
    let v2 = round v2
    if v1.x > v2.x then
        getLinePoints
            { line with
                Start = line.End
                End = line.Start }
    elif v1.x = v2.x then
        [ min v1.y v2.y .. max v1.y v2.y ]
        |> List.map (fun y -> Vec v1.x y)
    elif v1.y = v2.y then
        [ v1.x .. v2.x ]
        |> List.map (fun x -> Vec x v1.y)
    else
        let dy = v2.y - v1.y |> Math.Sign |> float
        let line = Line.init v1 <| v2 - v1
        let rec draw x y points =
            if x > v2.x || (dy > 0.0 && y > v2.y) || (dy < 0.0 && y < v2.y) then points
            else
                let points = Vec x y :: points
                let (x, y) =
                    [ (x, y + dy); (x + 1.0, y + dy); (x + 1.0, y) ]
                    |> List.minBy (fun (x1, y1) -> Line.sqDistanceToPoint (Vec x1 y1) line)
                draw x y points
        draw v1.x v1.y []

let drawLineOn (bitmap:Bitmap) line =
    let invertY = bitmap.Height - 1
    line
    |> getLinePoints
    |> List.iter
        (fun v ->
            bitmap.SetPixel (int v.x, invertY - int v.y, line.Color))

let draw3dShape (bitmap:Bitmap) center shape =
    shape
    |> to2d
    |> Shape.translate ((+) center)
    |> List.iter (drawLineOn bitmap)
    bitmap
    
let circle radius =
    let reflect f points =
        points
        |> List.map f
        |> List.append points
    let rec draw x y err points =
        if y <= x then
            let points = (x, y) :: points
            let y = y + 1
            if err <= 0 then
                draw x y (err + 2 * y + 1) points
            else
                let x = x - 1
                draw x y (err + 2 * (y - x) + 1) points
        else points
    draw radius 0 (1 - radius) []
    |> reflect (fun (x, y) -> (y, x))
    |> reflect (fun (x, y) -> (-x, y))
    |> reflect (fun (x, y) -> (x, -y))
    
let createSprites poses rotations createBase center width height =
    let newFrame () = createBase width height
    let yaw degrees =
        Quaternion.CreateFromYawPitchRoll (degrees |> degreesToRadians |> float32, 0.0f, 0.0f)
        |> Shape.transform
    let allRotations frames =
        rotations
        |> List.map
            (fun rotation ->
                frames
                |> List.map
                    (fun frame -> frame |> yaw rotation |> draw3dShape (newFrame()) center))
    poses
    |> List.map allRotations
    |> List.reduce List.append
        
let folder = Path.Combine (Environment.GetFolderPath Environment.SpecialFolder.Desktop, "Sprites")

let createSpritesheet poses rotations createBase center width height file =
    let sprites = createSprites poses rotations createBase center width height
    let w =
        sprites
        |> List.map List.length
        |> List.max
        |> (*) width
    let h = height * List.length sprites
    use bitmap = new Bitmap (w, h)
    for x in 0 .. w - 1 do
        for y in 0 .. h - 1 do
            bitmap.SetPixel (x, y, Color.Transparent)
    sprites
    |> List.iteri
        (fun i arr ->
            arr
            |> List.iteri
                (fun j img ->
                    for x in 0 .. width - 1 do
                        for y in 0 .. height - 1 do
                            bitmap.SetPixel (x + j * width, y + i * height, img.GetPixel (x, y))))
    bitmap.Save (Path.Combine(folder, file), Imaging.ImageFormat.Png)

let createCircleBitmap width height =
    let bitmap = createTransparentBitmap width height
    let r = width / 2
    for (x, y) in circle r do
        bitmap.SetPixel (x + r, y + height - r - 1, Color.Green)
    bitmap

let createPlayerSpritesheet () =
    let playerRotations =
        [ Rotation.Up; Rotation.UpLeft; Rotation.Left; Rotation.DownLeft;
          Rotation.Down; Rotation.DownRight; Rotation.Right; Rotation.UpRight ]
    let playerPoses =
        [ [ Body.standing ]
          Body.createWalkCycle Body.standing ]
        |> List.map (List.map Body.toShape)
    createSpritesheet
        playerPoses
        playerRotations
        createCircleBitmap
        (Vector3 (10.0f, 10.0f, 0.0f))
        21
        60
        "Player.png"

let createBodyPartSpritesheet file getBodyPart =
    let playerRotations =
        [ Rotation.Up; Rotation.UpLeft; Rotation.Left; Rotation.DownLeft;
          Rotation.Down; Rotation.DownRight; Rotation.Right; Rotation.UpRight ]
    let playerPoses =
        [ [ Body.standing ]
          Body.createWalkCycle Body.standing ]
        |> List.map (List.map getBodyPart)
    createSpritesheet
        playerPoses
        playerRotations
        createCircleBitmap
        (Vector3 (10.0f, 10.0f, 0.0f))
        21
        60
        file

let addDimension v shape : Shape =
    let shape1 = Shape.translate ((+) v) shape
    let shape2 = Shape.translate ((+) -v) shape
    List.zip shape1 shape2
    |> List.collect
        (fun (lineA, lineB) ->
            [ lineA
              lineB
              { lineA with End = lineB.Start }
              { lineB with Start = lineA.End } ])

let createPlankFenceSpritesheet () =
    let fencePoses =
        let post =
            [ Line.make Vector3.Zero (Vector3 (0.0f, 40.0f, 0.0f)) Color.Black ]
            |> addDimension (Vector3 (5.0f, 0.0f, 0.0f))
            |> addDimension (Vector3 (0.0f, 0.0f, 10.0f))
        let shift = Vector3 (12.0f, 0.0f, 0.0f)
        let fence : Shape =
            [ post
              (post |> Shape.translate ((+) shift))
              (post |> Shape.translate ((+) -shift))
              (post |> Shape.translate ((+) (2.0f * shift)))
              (post |> Shape.translate ((+) -(2.0f * shift))) ]
            |> List.collect id
        [ [ fence ] ]
    createSpritesheet
        fencePoses
        [ Rotation.Up ]
        createTransparentBitmap
        (Vector3 (31.0f, 2.0f, 0.0f))
        62
        60
        "Fence.png"

let createFireCircleSpritesheet () =
    let radius = 10
    let width = radius * 2 + 1
    let height = 30
    let maxFrequency = 3
    let maxFuncs = 3
    let minHeight = 2.0
    let variance = 4.0
    let frameCount = 20
    let file = "Teleporter.png"

    let sprites =
        let circle =
            let rng = Random ()
            let randomSine () =
                let a = rng.Next maxFrequency + 1 |> float
                let b = rng.NextDouble () * 2.0 * Pi
                fun x -> sin (x * a + b)
            let randomFunc () =
                let n = rng.Next maxFuncs + 1
                let funcs =
                    [ 1 .. n ]
                    |> List.map (fun _ -> randomSine ())
                fun x ->
                    List.averageBy (fun f -> f x) funcs
                    |> (+) 1.0
                    |> (*) (0.5 * variance)
                    |> (+) minHeight
                    |> Math.Round
                    |> int
            circle radius
            |> List.map (fun pt -> (pt, randomFunc ()))
        let adjustColor g =
            if g < 128 then
                Color.FromArgb (255, g * 2, 0, g * 2)
            else
                Color.FromArgb (255, 255, (g - 128) * 2, 255)
        [ 0 .. frameCount - 1 ]
        |> List.map (fun x -> (float x / float frameCount) * 2.0 * Pi)
        |> List.map
            (fun t ->
                let bitmap = createTransparentBitmap width height
                circle
                |> List.sortBy (fst >> snd)
                |> List.iter
                    (fun ((x, y), f) ->
                        let gray = 255 * (y + 2 * radius) / (4 * radius)
                        let x = x + radius
                        let y = y + height - radius - 1
                        let dy = f t
                        let color = adjustColor gray
                        for y in y - dy .. y do
                            bitmap.SetPixel (x, y, color))
                bitmap)
        |> (fun list -> [ list ])
    let w =
        sprites
        |> List.map List.length
        |> List.max
        |> (*) width
    let h = height * List.length sprites
    use bitmap = new Bitmap (w, h)
    for x in 0 .. w - 1 do
        for y in 0 .. h - 1 do
            bitmap.SetPixel (x, y, Color.Transparent)
    sprites
    |> List.iteri
        (fun i arr ->
            arr
            |> List.iteri
                (fun j img ->
                    for x in 0 .. width - 1 do
                        for y in 0 .. height - 1 do
                            bitmap.SetPixel (x + j * width, y + i * height, img.GetPixel (x, y))))
    bitmap.Save (Path.Combine(folder, file), Imaging.ImageFormat.Png)
    
[<EntryPoint>]
let main _ =
    Directory.CreateDirectory folder |> ignore

    createPlayerSpritesheet ()
    createBodyPartSpritesheet "LeftLeg.png"
        (fun body -> body.LeftLeg |> Leg.toShape)
    createPlankFenceSpritesheet ()
    createFireCircleSpritesheet ()

    0
