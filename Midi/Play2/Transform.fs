module Transform

open Midi
open Structure
open Util

let noteMinMax events =
    let min =
        events
        |> List.map
            (function
             | NoteOn (_, n, _) -> n
             | _ -> 127)
        |> List.min
    let max =
        events
        |> List.map
            (function
             | NoteOn (_, n, _) -> n
             | _ -> 0)
        |> List.max
    (min, max)

let trackNoteBounds =
    Track.events
    >> noteMinMax
    
type Data =
    | On
    | HoldD
    | RestD

module Transforms =
    let trackToData track =
        let gcd = 1 //trackGcd track
        let (minNote, maxNote) =
            let (minNote, maxNote) = trackNoteBounds track
            if minNote > maxNote then (minNote, minNote)
            else (minNote, maxNote)
        let ticks =
            track
            |> Track.events
            |> List.fold
                (fun ticks ->
                    function
                    | NoteOn (_, _, offset) | NoteOff (_, _, offset) ->
                        ticks + int offset
                    | _ -> ticks)
                0
        let data = Array2D.create (ticks / gcd + 1) (maxNote - minNote + 1) RestD
        track
        |> Track.events
        |> List.fold
            (fun lastTick event ->
                let offset =
                    match event with
                    | NoteOn (_, _, offset) -> int offset
                    | NoteOff (_, _, offset) -> int offset
                    | _ -> 0
                for i in lastTick + 1 .. lastTick + offset / gcd do
                    for j in 0 .. maxNote - minNote do
                        let color =
                            let last = data.[i - 1, j]
                            if last = On then HoldD
                            else last
                        data.[i, j] <- color
                match event with
                | NoteOn (_, note, _) ->
                    data.[lastTick + offset / gcd, note - minNote] <- On
                | NoteOff (_, note, _) ->
                    if note > 0 then
                        data.[lastTick + offset / gcd, note - minNote] <- RestD
                | _ -> ()
                lastTick + offset / gcd)
            0
        |> ignore
        data

    let dataToTrack name channel instrument tempo numerator denominator noteOffset noteLength (data:Data[,]) =
        let mutable track =
            Track.make name
            |> Track.setChannelInstrument channel instrument
            |> Track.setTempo tempo
            |> Track.setTimeSignature numerator denominator
        let addEvent event =
            track <- Track.addEvent event track
        for t in 0 .. data.GetLength 0 - 1 do
            for n in 0 .. data.GetLength 1 - 1 do
                let note = n + noteOffset
                match data.[t, n] with
                | On -> NoteOn (channel, note, 0uy) |> addEvent
                | HoldD -> ()
                | RestD ->
                    if t > 0 && data.[t - 1, n] <> RestD then
                        NoteOff (channel, note, 0uy) |> addEvent
            if t < data.GetLength 0 - 1 then
                NoteOff (channel, 0, noteLength) |> addEvent
        track

    let private dataTransform (data:Data[,]) =
        let getNotesOn t =
            data.[t, *]
            |> Array.mapi (fun i n -> (n, i))
            |> Array.choose (fun (n, i) -> match n with | On -> Some i | _ -> None)
            |> Array.toList
        let isContinued n1 n2 =
            match (n1, n2) with
            | (On, HoldD)
            | (HoldD, HoldD)
            | (RestD, HoldD)
            | (RestD, RestD) -> true
            | (On, On)
            | (On, RestD)
            | (HoldD, On)
            | (HoldD, RestD)
            | (RestD, On) -> false

        seq {
            let mutable last = getNotesOn 0
            let mutable len = 1
            for t in 1 .. data.GetLength 0 - 1 do
                if Array.forall2 isContinued data.[t - 1, *] data.[t, *] then
                    len <- len + 1
                else
                    yield (last, len)
                    last <- getNotesOn t
                    len <- 1
            yield (last, len)
        }
        |> Seq.toList

    let dataToNoteList (data:Data[,]) =
        if Array2D.length1 data = 0 || Array2D.length2 data = 0 then []
        else
            let notesAndLength =
                data
                |> dataTransform
                |> List.map
                    (fun (notes, length) ->
                        match notes with
                        | [] -> (Rest, length)
                        | notes -> (List.max notes |> Pitch.make |> Note, length))
            notesAndLength
            |> List.collect
                (fun (note, length) ->
                    note :: List.replicate (length - 1) Hold)

    let noteListToData (notes:Note list) : Data[,] =
        match notes with
        | [] -> Array2D.create 0 0 RestD
        | _ ->
            let (minNote, maxNote) =
                let noteValues =
                    notes
                    |> List.choose
                        (function
                            | Note n -> n |> int |> Some
                            | Hold | Rest -> None)
                (List.min noteValues, List.max noteValues)
            let data = Array2D.create (List.length notes) (maxNote - minNote + 1) RestD
            notes
            |> List.fold
                (fun (time, lastNote) ->
                    function
                    | Note n ->
                        data.[time, int n - minNote] <- On
                        (time + 1, int n - minNote)
                    | Hold ->
                        data.[time, lastNote] <- HoldD
                        (time + 1, lastNote)
                    | Rest -> (time + 1, lastNote))
                (0, 0)
            |> ignore
            data
