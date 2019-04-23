namespace Tests

open Deformation
open FsCheck
open Util
open Util.Geometry

type Float1M = Float1M of float with
    static member op_Explicit (Float1M x) = x

type Vector1M = Vector1M of Vector with
    static member op_Explicit (Vector1M v) = v

type Triangle1M = Triangle1M of Triangle2 with
    static member op_Explicity (Triangle1M t) = t

type Generators =
    static member Float1M () =
        Arb.from<float>
        |> Arb.filter (fun x -> (System.Double.IsInfinity x || System.Double.IsNaN x) |> not)
        |> Arb.convert
            (fun x -> x % 10000.0 |> Float1M)
            float
    static member Vector1M () =
        Arb.from<Float1M * Float1M>
        |> Arb.convert
            (fun (Float1M a, Float1M b) -> Vec a b |> Vector1M)
            (fun (Vector1M v) -> (Float1M v.x, Float1M v.y))
    static member Triangle1M () =
        Arb.from<Vector1M * Vector1M * Vector1M>
        |> Arb.convert
            (fun (Vector1M a, Vector1M b, Vector1M c) ->
                let poly = Polygon.make [ a; b; c ]
                if Polygon.isCounterClockwise poly then
                    { Triangle2.A = a; B = b; C = c }
                else
                    { Triangle2.A = a; B = c; C = b }
                |> Triangle1M)
            (fun (Triangle1M t) -> (Vector1M t.A, Vector1M t.B, Vector1M t.C))

type TriangleTransform =
    static member ``identity transform`` (Triangle1M triangle) (Vector1M point) =
        match Triangle2.tryCreateTransformTo triangle triangle with
        | Success transform -> point = transform point
        | Failure _ -> true
    static member ``inside points remain inside`` (Triangle1M triangleA) (Triangle1M triangleB) (Vector1M pointA) =
        match triangleA |> Triangle2.tryCreateTransformTo triangleB with
        | Success transform ->
            let pointB = transform pointA
            let polygonA = Triangle2.toPolygon triangleA
            let polygonB = Triangle2.toPolygon triangleB
            Polygon.contains pointA polygonA = Polygon.contains pointB polygonB
        | Failure _ -> true
