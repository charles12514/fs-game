namespace Deformation

open System
open System.Drawing
open System.Numerics
open Util
open Util.Geometry
open Util.Util

[<NoComparison>]
type BitmapSource =
    | File of string
    | Bitmap of Bitmap

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module BitmapSource =
    /// Callers are responsible for disposing
    let getBitmap =
        function
        | File file -> new Bitmap (file)
        | Bitmap bitmap -> bitmap

[<NoComparison>]
type Mesh =
  { Triangles : (Triangle2 * Triangle3) list
    BitmapSource : BitmapSource }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Mesh =
    let transform transform mesh =
        { mesh with
            Triangles =
                mesh.Triangles
                |> List.map (mapSnd (Triangle3.transform transform)) }
    /// Rectangle will be centered at the origin
    let loadRectangleMesh height width depth bitmapSource =
        let triangles =
            let t2 =
                let triangles x y dx dy =
                    let t1 =
                      { Triangle2.A = Vec (float x) (float y)
                        B = Vec (float x) (y + dy |> float)
                        C = Vec (x + dx |> float) (float y) }
                    let t2 =
                      { Triangle2.A = Vec (x + dx |> float) (float y)
                        B = Vec (float x) (y + dy |> float)
                        C = Vec (x + dx |> float) (y + dy |> float) }
                    (t1, t2)
                [
                    triangles 0 depth depth height // right
                    triangles depth depth width height // front
                    triangles depth 0 width depth // top
                    triangles depth (depth + height) width depth // bottom
                    triangles (depth + width) depth depth height // left
                    triangles (depth + width + depth) depth width height // back
                ]
            let t3 =
                let triangles pt d1 d2 =
                    let t1 =
                      { Triangle3.A = pt
                        B = pt + d2
                        C = pt + d1 }
                    let t2 =
                      { Triangle3.A = pt + d1
                        B = pt + d2
                        C = pt + d1 + d2 }
                    (t1, t2)
                let w = Vector3.make (float32 width) 0.0f 0.0f
                let h = Vector3.make 0.0f (float32 height) 0.0f
                let d = Vector3.make 0.0f 0.0f (float32 depth)
                [
                    triangles (Vector3.make (float32 -width / 2.0f) (float32 height / 2.0f) (float32 depth / 2.0f)) -d -h // right
                    triangles (Vector3.make (float32 -width / 2.0f) (float32 height / 2.0f) (float32 -depth / 2.0f)) w -h // front
                    triangles (Vector3.make (float32 -width / 2.0f) (float32 height / 2.0f) (float32 depth / 2.0f)) w -d // top
                    triangles (Vector3.make (float32 -width / 2.0f) (float32 -height / 2.0f) (float32 -depth / 2.0f)) w d // bottom
                    triangles (Vector3.make (float32 width / 2.0f) (float32 height / 2.0f) (float32 -depth / 2.0f)) d -h // left
                    triangles (Vector3.make (float32 width / 2.0f) (float32 height / 2.0f) (float32 depth / 2.0f)) -w -h // back
                ]
            List.zip t2 t3
            |> List.collect (fun ((t2a, t2b), (t3a, t3b)) -> [ (t2a, t3a); (t2b, t3b) ])
        { Triangles = triangles
          BitmapSource = bitmapSource }
    /// Positive angle means counter-clockwise
    let yaw (angle:float) =
        let rotation = Quaternion.CreateFromYawPitchRoll (float32 -angle, 0.0f, 0.0f)
        transform (fun v -> Vector3.Transform (v, rotation))
    /// Positive angle means counter-clockwise
    let pitch (angle:float) =
        let rotation = Quaternion.CreateFromYawPitchRoll (0.0f, float32 -angle, 0.0f)
        transform (fun v -> Vector3.Transform (v, rotation))
    /// Positive angle means counter-clockwise
    let roll (angle:float) =
        let rotation = Quaternion.CreateFromYawPitchRoll (0.0f, 0.0f, float32 -angle)
        transform (fun v -> Vector3.Transform (v, rotation))
    /// (xCenter, yCenter) indicates the pixel on the sprite image ((0, 0) being the bottom-left) that corresponds to the origin in mesh coordinates
    let toSprite (width:int) (height:int) (xCenter:int) (yCenter:int) (mesh:Mesh) =
        use source = BitmapSource.getBitmap mesh.BitmapSource
        let tri =
            mesh.Triangles
            |> List.choose
                (fun (spriteTriangle, meshTriangle) ->
                    if Vector3.Cross(meshTriangle.B - meshTriangle.A, meshTriangle.C - meshTriangle.A).Z |> float |> gt0 then
                        let flat =
                          { Triangle2.A = Vector3.collapseZ meshTriangle.A
                            B = Vector3.collapseZ meshTriangle.B
                            C = Vector3.collapseZ meshTriangle.C }
                        match flat |> Triangle2.tryCreateTransformTo spriteTriangle with
                        | Success transform -> Some (meshTriangle, flat, transform)
                        | Failure _ -> None
                    else None)
        let tryPixel (x:int) (y:int) =
            let x = float x + 0.5 - float xCenter
            let y = float height - (float y + 0.5) - float yCenter
            let v = Vec x y
            tri
            |> List.filter // triangles that the ray passes through
                (fun (_, t, _) ->
                    (t.B - t.A) |> Vector.cross (v - t.A) |> geq0
                    && (t.C - t.B) |> Vector.cross (v - t.B) |> geq0
                    && (t.A - t.C) |> Vector.cross (v - t.C) |> geq0)
            |> List.choose // get intersection with 3d triangles
                (fun (meshTriangle, _, transform) ->
                    Plane.make meshTriangle.A (Vector3.Cross (meshTriangle.B - meshTriangle.A, meshTriangle.C - meshTriangle.A))
                    |> Plane.lineIntersection (Vector3.make (float32 x) (float32 y) 0.0f) -Vector3.UnitZ
                    |> Option.map (fun intersection -> (transform, intersection)))
            |> List.sortBy // closest to camera
                (fun (_, intersection) -> intersection.Z)
            |> List.tryHead
            |> Option.map // map to bitmap and get pixel
                (fun (transform, intersection) ->
                    let pt =
                        intersection
                        |> Vector3.collapseZ
                        |> transform
                    source.GetPixel (int pt.x, int pt.y))
        let out = new Bitmap (width, height)
        for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                tryPixel x y
                |> Option.fold
                    (fun _ c -> c)
                    Color.Transparent
                |> fun c -> out.SetPixel (x, y, c)
        out
