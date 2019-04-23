open Midi
open System
open System.Drawing
open System.IO
open Util

let debug = Diagnostics.Debug.WriteLine
let desktop = Environment.SpecialFolder.Desktop |> Environment.GetFolderPath

let trackGcd track =
    let lengths =
        track
        |> Track.events
        |> List.choose
            (function
             | NoteOn (_, _, offset) | NoteOff (_, _, offset) when offset > 0uy ->
                offset |> int |> Some
             | _ -> None)
        |> List.countBy id
        |> List.filter
            (fun (len, n) -> len > 1 && n > 9)
    lengths
    |> List.sort
    |> List.iter
        (fun (a, b) ->
            sprintf "L = %i, count = %i" a b |> debug)
    let gcd =
        lengths
        |> List.map fst
        |> List.reduce Util.gcd
    sprintf "gcd = %i" gcd |> debug
    gcd

let songGcd song =
    song
    |> Song.tracks
    |> List.map trackGcd
    |> List.reduce Util.gcd

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

let noteBounds =
    Song.tracks
    >> List.collect Track.events
    >> noteMinMax

type Data =
    | On
    | Hold
    | Rest

module Transforms =
    let trackToData track =
        let gcd = 1 //trackGcd track
        let (minNote, maxNote) = trackNoteBounds track
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
        let data = Array2D.create (ticks / gcd + 1) (maxNote - minNote + 1) Rest
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
                            if last = On then Hold
                            else last
                        data.[i, j] <- color
                match event with
                | NoteOn (_, note, _) ->
                    data.[lastTick + offset / gcd, note - minNote] <- On
                | NoteOff (_, note, _) ->
                    if note > 0 then
                        data.[lastTick + offset / gcd, note - minNote] <- Rest
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
                | Hold -> ()
                | Rest ->
                    if t > 0 && data.[t - 1, n] <> Rest then
                        NoteOff (channel, note, 0uy) |> addEvent
            if t < data.GetLength 0 - 1 then
                NoteOff (channel, 0, noteLength) |> addEvent
        track

    let dataToBmp (data:Data[,]) =
        let bitmap = new Bitmap (data.GetLength 0, data.GetLength 1)
        for i in 0 .. data.GetLength 0 - 1 do
            for j in 0 .. data.GetLength 1 - 1 do
                let color =
                    match data.[i, j] with
                    | On -> Color.Black
                    | Hold -> Color.Gray
                    | Rest -> Color.White
                bitmap.SetPixel (i, data.GetLength 1 - 1 - j, color)
        bitmap

    let bmpToData (bitmap:Bitmap) =
        let data = Array2D.create bitmap.Width bitmap.Height Rest
        for i in 0 .. bitmap.Width - 1 do
            for j in 0 .. bitmap.Height - 1 do
                let color =
                    let p = bitmap.GetPixel (i, j)
                    if p.R = 0uy then On
                    elif p.R = 0xFFuy then Rest
                    else Hold
                data.[i, bitmap.Height - 1 - j] <- color
        data

    let imgFileToBmp (path:string) =
        new Bitmap (path)

    let bmpToImgFile path (bitmap:Bitmap) =
        bitmap.Save path

    let trackToMidi path ticksPerQtr track =
        use stream = new FileStream (path, FileMode.Create)
        Song.init ticksPerQtr
        |> Song.addTrack track
        |> Song.write stream
        stream.Flush ()

    let midiToTrack path =
        use stream = new FileStream (path, FileMode.Open)
        Song.tryMake stream
        |> function
           | Success song -> song |> Song.tracks |> List.head
           | Failure f -> failwith f

open Transforms

let drawTrack track =
    if track |> Track.events |> List.exists (function | NoteOn _ | NoteOff _ -> true | _ -> false) then
        let trackName = Track.name track
        let trackName =
            if String.IsNullOrEmpty trackName then
                let guid = Guid.NewGuid()
                guid.ToString ()
            else trackName
        trackName |> debug
        let name = Path.Combine (desktop, trackName + ".bmp")
        use bitmap =
            track
            |> trackToData
            |> dataToBmp
        bitmap.Save (name, Imaging.ImageFormat.Bmp)
    
let testDrawSong file =
    use stream = new FileStream (file, FileMode.Open)
    match Song.tryMake stream with
    | Success song ->
        song |> Song.ticksPerQuarter |> sprintf "%i ticks per quarter" |> debug
        song
        |> Song.setTicksPerQuarter (int Duration.Quarter)
        |> Song.tracks
        |> List.iter drawTrack
    | Failure f -> sprintf "Failure: %s" f |> debug

let readBytes (stream:Stream) =
    let rec read bytes =
        let next = stream.ReadByte ()
        if next = -1 then List.rev bytes
        else
            byte next :: bytes |> read
    read []

let dataTransform (data:Data[,]) =
    let getNotesOn t =
        data.[t, *]
        |> Array.mapi (fun i n -> (n, i))
        |> Array.choose (fun (n, i) -> match n with | On -> Some i | _ -> None)
        |> Array.toList
    let isContinued n1 n2 =
        match (n1, n2) with
        | (On, Hold)
        | (Hold, Hold)
        | (Rest, Hold)
        | (Rest, Rest) -> true
        | (On, On)
        | (On, Rest)
        | (Hold, On)
        | (Hold, Rest)
        | (Rest, On) -> false

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
    }
    |> Seq.toList

let combineBmpToMidi path files =
    use stream = new FileStream (path, FileMode.Create)
    files
    |> List.fold
        (fun song (file, offset) ->
            Path.Combine (desktop, file + ".bmp")
            |> imgFileToBmp
            |> bmpToData
            |> dataToTrack file (Channel.make 0) ElectricGuitarJazz (Tempo.make 90) 4 4 offset 12uy
            |> Song.addTrack <| song)
        (Song.init 12)
    |> Song.write stream
    stream.Flush ()

[<EntryPoint>]
let main _ =
    let file = @"..\..\Samples\Explosions in the Sky - Your Hand in Mine.mid"

    use stream = new FileStream (file, FileMode.Open)
    match Song.tryMake stream with
    | Success song ->
        song |> Song.ticksPerQuarter |> sprintf "%i ticks per quarter" |> debug
        song
        |> Song.tracks
        |> List.iter
            (fun track ->
                track |> Track.name |> debug
                track
                |> Track.events
                |> List.iter
                    (function
                     | NoteOn _ | NoteOff _ -> ()
                     | evt -> evt |> sprintf "%A" |> debug)
                debug "")
    | Failure f -> sprintf "Failure: %s" f |> debug

//    combineBmpToMidi
//        (Path.Combine (desktop, "Your Hand in Mine.mid"))
//        [
//            "Melody", 50
//            "Backing Melody", 40
//            "Melody 2", 50
//        ]

//    let file = "Melody"
//
//    Path.Combine (desktop, file + ".mid")
//    |> midiToTrack
//    |> trackToData
//    |> dataToBmp
//    |> bmpToImgFile (Path.Combine (desktop, file + "_1.bmp"))

//    Path.Combine (desktop, file + ".bmp")
//    |> imgFileToBmp
//    |> bmpToData
////    |> dataTransform
////    |> sprintf "%A"
////    |> debug
//    |> dataToTrack file (Channel.make 0) ElectricGuitarJazz (Tempo.make 180) 4 4 50 1uy
//    |> trackToMidi (Path.Combine (desktop, file + ".mid")) 12

//    midiToTrack (Path.Combine (desktop, file + ".mid"))
//    |> trackToData
//    |> dataToBmp
//    |> bmpToImgFile (Path.Combine (desktop, file + "_1.bmp"))

    0
