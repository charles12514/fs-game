namespace Walk

open System.Numerics

module Vector3 =
    let make x y z = Vector3 (x, y, z)
    let reflectX (v:Vector3) = Vector3 (-v.X, v.Y, v.Z)

[<NoComparison>]
type Line =
  { Start : Vector3
    End : Vector3
    Color : System.Drawing.Color }

    override this.ToString () =
        sprintf "Line { Start = (%f, %f, %f); End = (%f, %f, %f); Color = %s }" this.Start.X this.Start.Y this.Start.Z this.End.X this.End.Y this.End.Z (this.Color.ToString ())

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Line =
    let make p1 p2 color =
        { Start = p1
          End = p2
          Color = color }
    let transform (quaternion:Quaternion) line =
        { line with
            Start = Vector3.Transform (line.Start, quaternion)
            End = Vector3.Transform (line.End, quaternion) }
    let translate translation line =
        { line with
            Start = translation line.Start
            End = translation line.End }

type Shape = Line list

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Shape =
    let transform quaternion : Shape -> Shape =
        List.map (Line.transform quaternion)
    let translate translation : Shape -> Shape =
        List.map (Line.translate translation)
    let toString (shape:Shape) =
        shape
        |> List.fold
            (fun state line -> sprintf "%s    %s\n" state (line.ToString ()))
            ""
        |> sprintf "Shape\n  [\n%s  ]"

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
    let projectLineOnto plane line =
        { line with
            Start = projectPointOnto plane line.Start
            End = projectPointOnto plane line.End }
    let projectShapeOnto plane : Shape -> Shape =
        List.map (projectLineOnto plane)
