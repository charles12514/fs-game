namespace MoCap

open System
open System.IO
open System.Numerics

type Axis =
    | X
    | Y
    | Z

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Axis =
    let ofChar =
        function
        | 'X' | 'x' -> X
        | 'Y' | 'y' -> Y
        | 'Z' | 'z' -> Z
        | unknown -> failwithf "Unrecognized axis %c" unknown
    let toVector =
        function
        | X -> Vector3.UnitX
        | Y -> Vector3.UnitY
        | Z -> Vector3.UnitZ

type Dof =
    | Translation of Axis
    | Rotation of Axis

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Dof =
    let ofString (s:string) =
        match s.ToLower () with
        | "tx" -> Translation X
        | "ty" -> Translation Y
        | "tz" -> Translation Z
        | "rx" -> Rotation X
        | "ry" -> Rotation Y
        | "rz" -> Rotation Z
        | unknown -> failwithf "Unrecognized DOF %s" unknown

[<NoComparison>]
type Root =
  { Order : Dof list
    Axis : Axis * Axis * Axis
    Position : Vector3
    Orientation : Vector3 }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Root =
    let parse lines =
        let lines =
            lines
            |> List.map (split ' ')
        let read name =
            lines
            |> List.find (fun line -> line.[0] = name)
            |> fun a -> a.[1 ..]
        let parseVector =
            function
            | [| x; y; z |] -> Vector3 (float32 x, float32 y, float32 z)
            | _ -> failwith "misformed vector"
        let order = read "order" |> Array.toList |> List.map Dof.ofString
        let axis =
            read "axis"
            |> fun s -> s.[0].ToCharArray ()
            |> Array.map Axis.ofChar
            |> function
                | [| x; y; z |] -> (x, y, z)
                | _ -> failwith "malformed axis"
        let position = read "position" |> parseVector
        let orientation = read "orientation" |> parseVector
        { Order = order
          Axis = axis
          Position = position
          Orientation = orientation }

[<NoComparison>]
type BoneData =
  { Id : int
    Name : string
    Direction : Vector3
    Length : float32
    Axis : Vector3
    Dof : Dof list
    Limits : (float32 * float32) list }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module BoneData =
    let private parseBone lines =
        let lines =
            lines
            |> List.map (split ' ')
        let read name f =
            lines
            |> List.find (fun line -> line.[0] = name)
            |> fun a -> a.[1 ..]
            |> f
        let parseVector =
            function
            | [| x; y; z |] -> Vector3 (float32 x, float32 y, float32 z)
            | _ -> failwith "misformed vector"
        let id = read "id" (Array.item 0 >> int)
        let name = read "name" (Array.item 0)
        let direction = read "direction" parseVector |> Vector3.Normalize
        let length = read "length" (Array.item 0 >> float32)
        let axis = read "axis" (fun a -> a.[0 .. 2] |> parseVector)
        let dof =
            lines
            |> List.tryFind (fun line -> line.[0] = "dof")
            |> function
                | Some a -> a.[1 ..] |> Array.toList |> List.map Dof.ofString
                | None -> []
        let limits =
            lines
            |> List.filter
                (fun line -> line.[0] = "limits" || line.[0].StartsWith "(")
            |> List.map
                (fun line ->
                    let lower =
                        line
                        |> Array.find (fun x -> x.StartsWith "(")
                        |> fun x -> x.Substring 1
                        |> float32
                    let upper =
                        line
                        |> Array.find (fun x -> x.EndsWith ")")
                        |> fun x -> x.Substring (0, x.Length - 1)
                        |> float32
                    (lower, upper))
        { Id = id
          Name = name
          Direction = direction
          Length = length
          Axis = axis
          Dof = dof
          Limits = limits }
    let parse =
        List.fold
            (fun (nextBone, bonedatas) line ->
                if line = "begin" then ([], bonedatas)
                elif line = "end" then ([], (nextBone |> List.rev |> parseBone) :: bonedatas)
                else (line :: nextBone, bonedatas))
            ([], [])
        >> snd
        >> List.rev

type Hierarchy =
  { Parent : string
    Child : string }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Hierarchy =
    let parse =
        List.filter (fun line -> line <> "begin" && line <> "end")
        >> List.collect
            (split ' '
             >> Array.toList
             >> function
                | [] -> []
                | parent :: children ->
                    children
                    |> List.map
                        (fun child ->
                            { Parent = parent
                              Child = child }))

[<NoComparison>]
type Skeleton =
  { Root : Root
    BoneData : BoneData list
    Hierarchy : Hierarchy list }

[<NoComparison>]
type Piece =
    | Root of Root
    | Bone of BoneData

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Skeleton =
    [<NoComparison>]
    type private Directive =
        | Root of Root
        | BoneData of BoneData list
        | Hierarchy of Hierarchy list
    let private directive =
        function
        | ":root" :: rootLines -> rootLines |> Root.parse |> Root |> Some
        | ":bonedata" :: bonedataLines -> bonedataLines |> BoneData.parse |> BoneData |> Some
        | ":hierarchy" :: hierarchyLines -> hierarchyLines |> Hierarchy.parse |> Hierarchy |> Some
        | _ -> None
    let parse (FilePath path) =
        let directives =
            path
            |> File.ReadAllLines
            |> Seq.map (fun s -> s.Trim ())
            |> Seq.foldBack
                (fun line (dir, processed) ->
                    if line.StartsWith ":" then
                        let processed =
                            line :: dir
                            |> directive
                            |> function
                                | Some directive -> directive :: processed
                                | None -> processed
                        ([], processed)
                    else
                        (line :: dir, processed))
                <| ([], [])
            |> snd
        { Root =
            directives
            |> List.pick
                (function
                 | Root root -> Some root
                 | _ -> None)
          BoneData =
            directives
            |> List.pick
                (function
                 | BoneData data -> Some data
                 | _ -> None)
          Hierarchy =
            directives
            |> List.pick
                (function
                 | Hierarchy hierarchy -> Some hierarchy
                 | _ -> None) }
    let parentOf bone skeleton =
        skeleton.Hierarchy
        |> List.pick
            (fun { Parent = parent; Child = child } ->
                if child = bone.Name then Some parent
                else None)
        |> fun parentName ->
            if parentName = "root" then
                skeleton.Root |> Piece.Root
            else
                skeleton.BoneData
                |> List.find (fun bonedata -> bonedata.Name = parentName)
                |> Bone
