open MoCap
open System
open System.Collections.Generic
open System.Numerics

// http://research.cs.wisc.edu/graphics/Courses/cs-838-1999/Jeff/ASF-AMC.html
// http://mocap.cs.cmu.edu/search.php?subjectnumber=%&motion=%

let debug = Diagnostics.Debug.WriteLine

let toRadians angle =
    angle / 180.0f * float32 Math.PI

let getDof n =
    function
    | Translation axis -> (+) (Axis.toVector axis * n)
    | Rotation axis ->
        fun v -> Vector3.Transform (v, Quaternion.CreateFromAxisAngle (Axis.toVector axis, toRadians n))

let position (frame:Frame) (skeleton:Skeleton) =
    let rec positionBone =
        function
        | Root root ->
            (frame.Placements |> List.find (fun bone -> bone.Bone = "root")).Offsets
            |> List.zip root.Order
            |> List.fold
                (fun transform (dof, n) ->
                    transform >> getDof n dof)
                id
        | Bone bone ->
            let parent = skeleton |> Skeleton.parentOf bone |> positionBone
            let rotationAxis =
                [ (Rotation X, bone.Axis.X)
                  (Rotation Y, bone.Axis.Y)
                  (Rotation Z, bone.Axis.Z) ]
            let C =
                rotationAxis
                |> List.fold
                    (fun transform (dof, n) ->
                        transform >> getDof n dof)
                    id
            let Cinv =
                rotationAxis
                |> List.rev
                |> List.fold
                    (fun transform (dof, n) ->
                        transform >> getDof -n dof)
                    id
            let M =
                frame.Placements
                |> List.tryFind (fun b -> b.Bone = bone.Name)
                |> function
                    | None -> id
                    | Some placement ->
                        placement.Offsets
                        |> List.zip bone.Dof
                        |> List.fold
                            (fun transform (dof, n) ->
                                transform >> getDof n dof)
                            id
            let B = (+) (bone.Direction * bone.Length)
            Cinv >> M >> C >> B >> parent
    let endpoint =
        Root skeleton.Root :: (skeleton.BoneData |> List.map Bone)
        |> List.map (fun b -> (b, positionBone b Vector3.Zero))
        |> dict
    endpoint.Keys
    |> Seq.map
        (function
         | Root _ as root -> (root, (endpoint.[root], endpoint.[root]))
         | Bone b as bone -> (bone, (endpoint.[Skeleton.parentOf b skeleton], endpoint.[bone])))
    |> Seq.toList
    
module Sprite =
    open System.Drawing
    open Util.Geometry

    let createTransparentBitmap (width:int) height =
        let bitmap = new Bitmap (width, height)
        for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                bitmap.SetPixel (x, y, Color.Transparent)
        bitmap

    let rec getLinePoints a b =
        let round (n:float32) = n |> float |> Math.Floor |> float32 |> (+) 0.5f
        let round (x, y) = (round x, round y)
        let (ax, ay) = round a
        let (bx, by) = round b
        if ax > bx then
            getLinePoints b a
        elif ax = bx then
            [ min ay by .. max ay by ]
            |> List.map (fun y -> (ax, y))
        elif ay = by then
            [ ax .. bx ]
            |> List.map (fun x -> (x, ay))
        else
            let dy = by - ay |> Math.Sign |> float32
            let line = Line.init <| Vec (float ax) (float ay) <| Vec (bx - ax |> float) (by - ay |> float)
            let rec draw x y points =
                if x > bx || (dy > 0.0f && y > by) || (dy < 0.0f && y < by) then points
                else
                    let points = (x, y) :: points
                    let (x, y) =
                        [ (x, y + dy); (x + 1.0f, y + dy); (x + 1.0f, y) ]
                        |> List.minBy (fun (x1, y1) -> Line.sqDistanceToPoint (Vec (float x1) (float y1)) line)
                    draw x y points
            draw ax ay []

    let drawLineOn (bitmap:Bitmap) a b color =
        let invertY = bitmap.Height - 1
        getLinePoints a b
        |> List.iter
            (fun (x, y) ->
                bitmap.SetPixel (int x, invertY - int y, color))

    let draw3dShape (bitmap:Bitmap) (transform:Vector3 -> Vector3) (lines:(Vector3 * Vector3 * Color) list)=
        lines
        |> Seq.map (fun (v1, v2, c) -> (transform v1, transform v2, c))
        |> Seq.map (fun (v1, v2, c) -> ((v1.X, v1.Y), (v2.X, v2.Y), c))
        |> Seq.iter (fun (v1, v2, c) -> drawLineOn bitmap v1 v2 c)
        bitmap

    let drawSkeleton (skeleton:(Piece * (Vector3 * Vector3)) list) =
        let rotate v =
            Vector3.Transform (v, Quaternion.CreateFromAxisAngle (Vector3.UnitY, Math.PI / 8.0 |> float32))
        let skeleton =
            skeleton
            |> List.map
                (fun (p, (a, b)) -> (p, (rotate a, rotate b)))
        let points =
            skeleton
            |> List.collect (fun (_, (a, b)) -> [ a; b ])
        let minMax f =
            points
            |> List.fold
                (fun (minF, maxF) pt ->
                    (min minF <| f pt, max maxF <| f pt))
                (1000.0f, -1000.0f)
        let (minX, maxX) = minMax (fun v -> v.X)
        let (minY, maxY) = minMax (fun v -> v.Y)
        let (minX, maxX) = (minX - 1.0f, maxX + 1.0f)
        let (minY, maxY) = (minY - 1.0f, maxY + 1.0f)
        let transform v =
            v - Vector3 (minX, minY, 0.0f)
        let width = (maxX - minX) |> float |> Math.Ceiling |> int
        let height = (maxY - minY) |> float |> Math.Ceiling |> int
        let color =
            function
            | Root _ -> "root"
            | Bone b -> b.Name
            >> function
                | "rhumerus" -> Color.Red
                | "lhumerus" -> Color.Blue
                | _ -> Color.Black
        skeleton
        |> List.map
            (fun (p, (a, b)) -> (a, b, color p))
        |> (draw3dShape
            <| createTransparentBitmap width height
            <| transform)

    let createSpritesheet1D (FilePath path) skeleton frames =
        let sprites =
            frames
            |> List.map (fun frame -> position frame skeleton)
            |> List.map drawSkeleton
        let width =
            sprites
            |> List.map (fun sprite -> sprite.Width)
            |> List.max
        let w = width * sprites.Length
        let height =
            sprites
            |> List.map (fun sprite -> sprite.Height)
            |> List.max
        let h = height
        use bitmap = new Bitmap (w, h)
        for x in 0 .. w - 1 do
            for y in 0 .. h - 1 do
                bitmap.SetPixel (x, y, Color.Transparent)
        sprites
        |> List.iteri
            (fun i img ->
                for x in 0 .. img.Width - 1 do
                    for y in 0 .. img.Height - 1 do
                        bitmap.SetPixel (x + i * width, y, img.GetPixel (x, y)))
        bitmap.Save (path, Imaging.ImageFormat.Png)

    let desktop = Environment.GetFolderPath Environment.SpecialFolder.Desktop

[<EntryPoint>]
let main _ =
    let skeleton = Skeleton.parse <| FilePath @"C:\Users\csnyder\Desktop\07.asf"
    let animation = Animation.parse <| FilePath @"C:\Users\csnyder\Desktop\07_02.amc"

    let frame = animation.Frames |> List.head
    let positions = position frame skeleton

    positions
    |> List.iter
        (fun (piece, (a, b)) ->
            match piece with
            | Root _ -> "root"
            | Bone b -> b.Name
            |> sprintf "%s : (%A, %A)" <| a <| b
            |> debug)

    animation.Frames
    |> List.filter (fun frame -> frame.Id % 10 = 1)
    |> Sprite.createSpritesheet1D (IO.Path.Combine (Sprite.desktop, "mocap test.png") |> FilePath) skeleton

    0
