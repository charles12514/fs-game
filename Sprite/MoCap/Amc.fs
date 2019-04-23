namespace MoCap

open System
open System.IO

type Placement =
  { Bone : string
    Offsets : float32 list }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Placement =
    let parse (line:string) =
        let line = line |> split ' '
        { Bone = line.[0]
          Offsets = line.[1 ..] |> Array.toList |> List.map float32 }

type Frame =
  { Id : int
    Placements : Placement list }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Frame =
    let parse id lines =
        { Id = id
          Placements =
            lines
            |> Seq.map Placement.parse
            |> Seq.toList }

type Animation =
  { Frames : Frame list }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Animation =
    let parse (FilePath path) =
        path
        |> File.ReadAllLines
        |> Seq.foldBack
            (fun line (currentFrame, frames) ->
                match Int32.TryParse line with
                | (true, id) ->
                    let frame = Frame.parse id currentFrame
                    ([], frame :: frames)
                | (false, _) ->
                    (line :: currentFrame, frames))
            <| ([], [])
        |> fun (_, frames) -> { Frames = frames }
