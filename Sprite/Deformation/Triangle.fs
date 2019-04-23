namespace Deformation

open System.Numerics
open Util
open Util.Geometry

module Vector3 =
    let make x y z = Vector3 (x, y, z)
    let reflectX (v:Vector3) = Vector3 (-v.X, v.Y, v.Z)
    let collapseX (v:Vector3) = Vec <| float v.Y <| float v.Z
    let collapseY (v:Vector3) = Vec <| float v.X <| float v.Z
    let collapseZ (v:Vector3) = Vec <| float v.X <| float v.Y

type Triangle2 =
  { A : Vector
    B : Vector
    C : Vector }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle2 =
    let toPolygon triangle =
        Polygon.make [ triangle.A; triangle.B; triangle.C ]

    let tryCreateTransformTo t2 t1 =
        let toMatrix t =
            let n = Array2D.create 3 3 1.0
            n.[0, 0] <- t.A.x
            n.[1, 0] <- t.A.y
            n.[0, 1] <- t.B.x
            n.[1, 1] <- t.B.y
            n.[0, 2] <- t.C.x
            n.[1, 2] <- t.C.y
            n
            |> Matrix.tryMake
            |> Attempt.getSuccess
        t1
        |> toMatrix
        |> Matrix.inverse
        |> Attempt.bind
            (fun nInv ->
                t2
                |> toMatrix
                |> Matrix.multiply nInv)
        |> Attempt.map
            (fun transform ->
                fun (v:Vector) ->
                    let v =
                        let n = Array2D.create 3 1 1.0
                        n.[0, 0] <- v.x
                        n.[1, 0] <- v.y
                        n
                        |> Matrix.tryMake
                        |> Attempt.getSuccess
                    let v' =
                        transform
                        |> Matrix.multiply v
                        |> Attempt.getSuccess
                    Vec v'.[0, 0] v'.[1, 0])

[<NoComparison>]
type Triangle3 =
  { A : Vector3
    B : Vector3
    C : Vector3 }
    
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle3 =
    let tryCreateTransformTo t2 t1 =
        let insert (m:float[,]) column (v:Vector3) =
            m.[0, column] <- float v.X
            m.[1, column] <- float v.Y
            m.[2, column] <- float v.Z
        let toMatrix t =
            let n = Vector3.Cross (t.B - t.A, t.C - t.A) |> Vector3.Normalize
            let m = Array2D.create 4 4 1.0
            insert m 0 t.A
            insert m 1 t.B
            insert m 2 t.C
            insert m 3 n
            m
            |> Matrix.tryMake
            |> Attempt.getSuccess
        t1
        |> toMatrix
        |> Matrix.inverse
        |> Attempt.bind
            (fun nInv ->
                t2
                |> toMatrix
                |> Matrix.multiply nInv)
        |> Attempt.map
            (fun transform ->
                fun v ->
                    let v =
                        let n = Array2D.create 4 1 1.0
                        insert n 0 v
                        n
                        |> Matrix.tryMake
                        |> Attempt.getSuccess
                    let v' =
                        transform
                        |> Matrix.multiply v
                        |> Attempt.getSuccess
                    Vector3.make (float32 v'.[0, 0]) (float32 v'.[1, 0]) (float32 v'.[2, 0]))
    let transform transform triangle =
        { Triangle3.A = transform triangle.A
          B = transform triangle.B
          C = transform triangle.C }

[<NoComparison>]
type Plane =
    private
      { Point : Vector3
        Normal : Vector3 }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Plane =
    let point plane = plane.Point
    let normal plane = plane.Normal
    let make point normal =
        { Point = point
          Normal = Vector3.Normalize normal }
    let projectPointOnto plane point =
        point - Vector3.Dot (point - plane.Point, plane.Normal) * plane.Normal
    let lineIntersection point dir plane =
        if Vector3.Dot (dir, plane.Normal) |> float |> eq0 then
            None // could be infinite, don't care
        else
            let d = Vector3.Dot (plane.Point - point, plane.Normal) / Vector3.Dot (dir, plane.Normal)
            d * dir + point |> Some
