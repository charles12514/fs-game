namespace Midi

open System.IO
open Util

type Song =
    private
      { ReversedTracks : Track list
        IsAsync : bool
        TicksPerQuarter : int }
    override this.ToString () =
        this.ReversedTracks |> List.rev |> sprintf "%A"

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Song =
    let init ticksPerQuarter =
        { ReversedTracks = []
          IsAsync = false
          TicksPerQuarter = ticksPerQuarter }
    let tracks song =
        song.ReversedTracks
        |> List.rev
    let ticksPerQuarter song =
        song.TicksPerQuarter
    let addTrack track song =
        { song with ReversedTracks = track :: song.ReversedTracks }
    let setTicksPerQuarter ticks song =
        { song with
            TicksPerQuarter = ticks
            ReversedTracks =
                song.ReversedTracks
                |> List.map
                    (Track.mapOffsets
                        (fun n ->
                            (int n * ticks |> float) / float song.TicksPerQuarter
                            |> System.Math.Round
                            |> byte)) }
    let toMetadata song =
        SongHeader (song.ReversedTracks.Length, song.IsAsync, song.TicksPerQuarter)
        :: (tracks song |> List.collect Track.toMetadata)
    let tryParse =
        let rec tryParse metadata =
            function
            | [] -> Success (List.rev metadata)
            | b ->
                match Metadata.tryParse b with
                | Success (token, b) -> tryParse (token :: metadata) b
                | Failure f -> Failure f
        tryParse []
    let fromMetadata =
        let rec readToTrackEnd track =
            function
            | [] -> Failure "Empty list"
            | TrackEnd :: data -> Success (List.rev track, data)
            | Event evt :: data -> readToTrackEnd (evt :: track) data
            | Unknown _ :: data -> readToTrackEnd track data
            | list -> sprintf "Unexpected list: %A" list |> Failure
        let readTrack =
            readToTrackEnd []
            >> Attempt.bind
                (fun (events, data) ->
                    events
                    |> Track.fromEvents
                    |> Attempt.bind (fun track -> Success (track, data)))
        let rec readTracks tracks =
            function
            | [] -> tracks |> List.rev |> Success
            | TrackStart _ :: data ->
                readTrack data
                |> Attempt.bind
                    (fun (track, data) ->
                        readTracks (track :: tracks) data)
            | _ -> Failure "Expected track start mark"
        function
        | SongHeader (_, isAsync, ticksPerQtr) :: tracks ->
            readTracks [] tracks
            |> Attempt.bind
                (fun tracks -> { ReversedTracks = List.rev tracks; IsAsync = isAsync; TicksPerQuarter = ticksPerQtr } |> Success)
        | _ -> Failure "Expected song header"
    let private readBytes (stream:Stream) =
        let rec read bytes =
            let next = stream.ReadByte ()
            if next = -1 then List.rev bytes
            else
                byte next :: bytes |> read
        read []
    let tryMake<'a when 'a :> Stream> = readBytes >> tryParse >> Attempt.bind fromMetadata
    let write (stream:Stream) song =
        let bytes = song |> toMetadata |> List.collect Metadata.toBytes
        for b in bytes do
            stream.WriteByte b
