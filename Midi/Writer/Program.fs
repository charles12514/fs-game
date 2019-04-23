open Midi
open System
open System.IO

type Sound =
  { Pitch : Pitch
    Duration : int }

type ChannelEvent =
    | Sound of Sound
    | SetInstrument of ChromaticInstrument
    | Aftertouch of Pitch * byte
    | ControlChange of byte * byte
    | ProgramChange of byte

type TrackEvent =
    | SetTempo of Tempo
    | SetTimeSignature of int * int
    | ChannelEvent of Channel * ChannelEvent
    
type UntimedEvent =
    | SetChannelInstrument of Channel * ChromaticInstrument
    | SetTempo of Tempo
    | SetTimeSignature of int * int
    | NoteOff of Channel * Pitch
    | NoteOn of Channel * Pitch
    | Aftertouch of Channel * Pitch * byte // note, pressure
    | ControlChange of Channel * byte * byte // controller number, controller value
    | ProgramChange of Channel * byte // program number

type AbsoluteTrackEvent =
  { TrackEvent : TrackEvent
    Time : int }

type AbsoluteEvent =
  { Event : UntimedEvent
    Time : int }

type RelativeEvent =
  { Event : UntimedEvent
    Offset : int }

let absoluteTrackToAbsolute =
    List.collect
        (fun { TrackEvent = event; Time = time } ->
            match event with
            | TrackEvent.SetTempo tempo -> [ { Event = SetTempo tempo; Time = time } ]
            | TrackEvent.SetTimeSignature (n, d) -> [ { Event = SetTimeSignature (n, d); Time = time } ]
            | ChannelEvent (channel, event) ->
                match event with
                | ChannelEvent.Aftertouch (pitch, pressure) -> [ { Event = Aftertouch (channel, pitch, pressure); Time = time }]
                | ChannelEvent.ControlChange (number, value) -> [ { Event = ControlChange (channel, number, value); Time = time }]
                | ChannelEvent.ProgramChange number -> [ { Event = ProgramChange (channel, number); Time = time }]
                | SetInstrument instrument -> [ { Event = SetChannelInstrument (channel, instrument); Time = time }]
                | Sound sound ->
                    [ { Event = NoteOn (channel, sound.Pitch); Time = time }
                      { Event = NoteOff (channel, sound.Pitch); Time = time + sound.Duration } ])

let absoluteToRelative =
    List.sortBy (fun event -> event.Time)
    >> function
        | [] -> []
        | (h :: _) as events ->
            let tail =
                events
                |> List.pairwise
                |> List.map
                    (fun ({ Event = _; Time = t1 }, { Event = event; Time = t2 }) ->
                        { Event = event; Offset = t2 - t1 })
            { Event = h.Event; Offset = h.Time } :: tail

let createOffsetEvents =
    let rec createOffsetEvents offset =
        if offset <= 0 then
            []
        elif offset <= int Byte.MaxValue then
            [ Event.NoteOff (Channel.make 0, 0, byte offset) ]
        else
            Event.NoteOff (Channel.make 0, 0, Byte.MaxValue) :: createOffsetEvents (offset - int Byte.MaxValue)
    createOffsetEvents

let relativeToMidi =
    List.collect
        (fun { Event = event; Offset = offset } ->
            let event =
                match event with
                | SetChannelInstrument (channel, instrument) ->
                    Event.SetChannelInstrument (channel, instrument)
                | SetTempo tempo ->
                    Event.SetTempo tempo
                | SetTimeSignature (n, d) ->
                    Event.SetTimeSignature (n, d)
                | NoteOff (channel, pitch) ->
                    Event.NoteOff (channel, int pitch, 0uy)
                | NoteOn (channel, pitch) ->
                    Event.NoteOn (channel, int pitch, 0uy)
                | Aftertouch (channel, pitch, pressure) ->
                    Event.Aftertouch (channel, int pitch, pressure, 0uy)
                | ControlChange (channel, number, value) ->
                    Event.ControlChange (channel, number, value, 0uy)
                | ProgramChange (channel, number) ->
                    Event.ProgramChange (channel, number, 0uy)
            createOffsetEvents offset @ [ event ])

let trackToMidi =
    absoluteTrackToAbsolute
    >> absoluteToRelative
    >> relativeToMidi

let printScale scale =
    for n in scale do
        printf "%O " n
    printfn ""
    let tuning = Neck.Tuning.standard
    for f in 0 .. Octave.length - 1 do
        printf "%3i    " f
    printfn ""
    Neck.find tuning scale
    |> List.zip tuning
    |> List.rev
    |> List.iter
        (fun (openString, frets) ->
            for f in 0 .. Octave.length - 1 do
                if frets |> Seq.contains f then
                    openString
                    |> NoteOctave.toPitch
                    |> (+) f
                    |> NoteOctave.ofPitch
                    |> sprintf "%3O"
                else "---"
                |> printf "%s|---"
            printfn "")

[<EntryPoint>]
let main _ =
    let track =
        let ch = [| 0 .. 3 |] |> Array.map Channel.make
        Track.make "Track 1"
        |> Track.setTempo (Tempo.make 90)
        |> Track.setTimeSignature 4 4
        |> Track.setChannelInstrument ch.[0] AcousticGrandPiano
        |> Track.setChannelInstrument ch.[1] ElectricBassFinger
        |> Track.setChannelInstrument ch.[2] AcousticGuitarSteel
        |> Track.setChannelInstrument ch.[3] Violin
        |> Track.addEvent (NoteOn (ch.[0], 50, 0uy))
        |> Track.addEvent (NoteOff (ch.[0], 50, Duration.Whole |> byte))
        |> Track.addEvent (NoteOn (ch.[1], 50, 0uy))
        |> Track.addEvent (NoteOff (ch.[1], 50, Duration.Whole |> byte))
        |> Track.addEvent (NoteOn (ch.[2], 50, 0uy))
        |> Track.addEvent (NoteOff (ch.[2], 50, Duration.Whole |> byte))
        |> Track.addEvent (NoteOn (ch.[3], 50, 0uy))
        |> Track.addEvent (NoteOff (ch.[3], 50, Duration.Whole |> byte))
    let filePath = Path.Combine (Environment.SpecialFolder.Desktop |> Environment.GetFolderPath, "miditest.mid")
    use fileStream = new FileStream (filePath, FileMode.Create)
    Song.init 12
    |> Song.addTrack track
    |> Song.write fileStream
    fileStream.Flush ()

    let scale = Chord.minor ``E``
    printScale scale
    
//    let sf2 =
//        let path = @"F:\Users\Charles\Desktop\FluidR3_GM.SF2"
//        use file = new FileStream (path, FileMode.Open)
//        let input =
//            seq {
//                let mutable i = file.ReadByte ()
//                while i <> -1 do
//                    yield byte i
//                    i <- file.ReadByte ()
//            }
//            |> Seq.toArray
//        SoundFont2.Parser.parse input None None None None
//
//    printfn "PRESETS"
//    printfn "-------"
//    sf2.PresetData.PresetHeaders
//    |> Seq.take 10
//    |> Seq.iter
//        (fun preset ->
//            printfn "%s" preset.PresetName)
//
//    printfn ""
//    printfn "INSTRUMENTS"
//    printfn "-----------"
//    sf2.PresetData.Instruments
//    |> Seq.take 10
//    |> Seq.iter
//        (fun instr ->
//            printfn "%s" instr.InstrumentName)
//            
//    printfn ""
//    printfn "SAMPLES"
//    printfn "-------"
//    sf2.PresetData.SampleHeaders
//    |> Seq.take 10
//    |> Seq.iter
//        (fun (_, sample) ->
//            printfn "%s" sample.SampleName)
//
//    printfn ""
//    printfn "PRESETS"
//    printfn "-------"
//    let presets = SoundFont2.Parser.getPresets sf2.PresetData
//    presets
//    |> Seq.take 10
//    |> Seq.iter
//        (fun p ->
//            printfn "%s" p.Name)
//            
//    printfn ""
//    printfn "INSTRS"
//    printfn "------"
//    let instruments = SoundFont2.Parser.getInstruments sf2.PresetData
//    instruments
//    |> Seq.take 10
//    |> Seq.iter
//        (fun i ->
//            printfn "%s" i.InstrumentName)
//
//    let guitar =
//        sf2.PresetData.SampleHeaders
//        |> Seq.find
//            (fun (_data, header) ->
//                header.SampleName.StartsWith "Steel A5")
//    guitar |> ignore

    0
