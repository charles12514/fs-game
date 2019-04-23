namespace Midi

open Util

type Metadata =
    | SongHeader of int * bool * int // track count, is async, ticks per quarter
    | TrackStart of uint64
    | TrackEnd
    | Event of Event
    | Unknown of byte

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Metadata =
    let toBytes =
        function
        | SongHeader (trackCount, isAsync, ticksPerQuarter) ->
            [ 0x4Duy
              0x54uy
              0x68uy
              0x64uy
              0x00uy
              0x00uy
              0x00uy
              0x06uy
              0x00uy
              (if trackCount > 1 then
                if isAsync then 0x02uy else 0x01uy
               else 0x00uy) ]
            @ (if trackCount < 0x80 then [ 0x00uy ] else [])
            @ VariableLength.toBytes (trackCount |> uint64)
            @ [ 0x00uy ]
            @ VariableLength.toBytes (ticksPerQuarter |> uint64)
        | TrackStart length ->
            [ 0x4Duy
              0x54uy
              0x72uy
              0x6Buy
              (length &&& 0xFF000000UL) >>> 24 |> byte
              (length &&& 0xFF0000UL) >>> 16 |> byte
              (length &&& 0xFF00UL) >>> 8 |> byte
              length &&& 0xFFUL |> byte ]
        | TrackEnd ->
            [ 0x00uy; 0xFFuy; 0x2Fuy; 0x00uy ]
        | Event event ->
            Event.toBytes event
        | Unknown x ->
            [ x ]
    let tryParse =
        function
        | 0x4Duy :: 0x54uy :: 0x68uy :: 0x64uy :: 0x00uy :: 0x00uy :: 0x00uy :: 0x06uy :: 0x00uy :: t ->
            // isAsync byte
            match t with
            | 0x02uy :: t -> Success (true, t)
            | 0x00uy :: t | 0x01uy :: t -> Success (false, t)
            | x -> sprintf "Failure parsing isAsync, expecting 0x00, 0x01, or 0x02, found %A" x |> Failure
            // optional 0x00 byte and track count
            |> Attempt.bind
                (fun (isAsync, t) ->
                    let t =
                        match t with
                        | 0x00uy :: t | t -> t
                    match VariableLength.tryParse t with
                    | Some x -> Success x
                    | None -> Failure "Failed parsing track count"
                    |> Attempt.bind (fun (trackCount, t) -> Success (isAsync, trackCount, t)))
            // ignore this byte
            |> Attempt.bind
                (fun (isAsync, trackCount, t) ->
                    match t with
                    | _ :: t -> Success (isAsync, trackCount, t)
                    | [] -> Failure "Failure reading ignored byte")
            // ticks per quarter
            |> Attempt.bind
                (fun (isAsync, trackCount, t) ->
//                    match VariableLength.tryParse t with
//                    | Some (ticksPerQtr, t) -> Success (isAsync, trackCount, int ticksPerQtr, t)
//                    | None -> Failure "Failure reading ticks per quarter"
                    match t with
                    | ticksPerQtr :: t -> Success (isAsync, trackCount, int ticksPerQtr, t)
                    | [] -> Failure "Failure reading ticks per quarter")
            // convert to SongHeader
            |> Attempt.bind
                (fun (isAsync, trackCount, ticksPerQtr, t) ->
                    (SongHeader (int trackCount, isAsync, ticksPerQtr), t) |> Success)
        | 0x4Duy :: 0x54uy :: 0x72uy :: 0x6Buy :: l1 :: l2 :: l3 :: l4 :: t ->
            let length =
                (uint64 l1 <<< 24)
                ||| (uint64 l2 <<< 16)
                ||| (uint64 l3 <<< 8)
                ||| uint64 l4
            Success (TrackStart length, t)
        | 0x00uy :: 0xFFuy :: 0x2Fuy :: 0x00uy :: t ->
            Success (TrackEnd, t)
        | h :: t ->
            match Event.tryParse (h :: t) with
            | Success (event, t) -> Success (Event event, t)
            | Failure _ -> Success (Unknown h, t)
        | [] ->
            Failure "Unexpected end of bytes"
