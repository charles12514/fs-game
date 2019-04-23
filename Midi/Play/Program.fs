open Midi
open Structure
open Structure.Parameters
open System

module Parameters =
    let seed = Random().Next()
    let length = int Duration.Whole * 28
    let scale = Scale.``C major``
    
    let modifyParams =
      { PRepeat = 0.2
        PReset = 0.15
        PShift = 0.5
        PNew = 0.15 }
    let shiftParams =
      { PUnison = 0.15
        PStepUp = 0.325
        PStepDown = 0.325
        PLeapUp = 0.1
        PLeapDown = 0.1
        MaxLeap = 8 }
    let instruments =
        [ { Name = "Bass"
            Instrument = Chromatic AcousticBass
            Scale = scale |> Scale.inOctaves 1 2
            TrackOptions =
                [ { PSelectOption = 1.0
                    Modify = modifyParams
                    Shift = shiftParams
                    MeterCreation = New (4, [ (1, 1.0) ], Duration.Whole, 12 |> Pitch.make |> Note)
                    Meter = { PNote = 1.0; PHold = 0.0; PRest = 0.0 }
                    PhraseRepeat =
                        [ (4, modifyParams, shiftParams)
                          (3, modifyParams, shiftParams) ] } ] }
          { Name = "Melody 1"
            Instrument = Chromatic AcousticGuitarNylon
            Scale = scale |> Scale.inOctaves 2 3
            TrackOptions =
                [ { PSelectOption = 1.0
                    Modify = modifyParams
                    Shift = shiftParams
                    MeterCreation = RelativeTo ("Bass", 4)
                    Meter = { PNote = 0.75; PHold = 0.25; PRest = 0.0 }
                    PhraseRepeat = [ (4, modifyParams, shiftParams) ] } ] }
          { Name = "Melody 2"
            Instrument = Chromatic AcousticGuitarNylon
            Scale = scale |> Scale.inOctaves 2 4
            TrackOptions =
                [ { PSelectOption = 1.0
                    Modify = modifyParams
                    Shift = shiftParams
                    MeterCreation = RelativeTo ("Bass", 4)
                    Meter = { PNote = 0.4; PHold = 0.3; PRest = 0.3 }
                    PhraseRepeat = [ (2, modifyParams, shiftParams) ] } ] }
          { Name = "Melody 3"
            Instrument = Chromatic AcousticGuitarNylon
            Scale = scale |> Scale.inOctaves 3 5
            TrackOptions =
                [ { PSelectOption = 0.8
                    Modify = modifyParams
                    Shift = shiftParams
                    MeterCreation = RelativeTo ("Melody 1", 2)
                    Meter = { PNote = 0.6; PHold = 0.25; PRest = 0.15 }
                    PhraseRepeat =
                        [ (2, { PRepeat = 0.1; PReset = 0.1; PShift = 0.2; PNew = 0.6 }, shiftParams) ] }
                  { PSelectOption = 0.2
                    Modify = modifyParams
                    Shift = shiftParams
                    MeterCreation = New (8, [ (8, 1.0) ], Duration.Eighth, Rest)
                    Meter = { PNote = 0.0; PHold = 0.0; PRest = 1.0 }
                    PhraseRepeat = [] } ] }
          { Name = "Bass Drum"
            Instrument = Percussion
            Scale = PercussionInstrumentClass.Bass |> PercussionInstrumentClass.toInstrumentList |> List.map (int >> Pitch.make) |> List.toArray
            TrackOptions =
                [ { PSelectOption = 1.0
                    Modify = { PRepeat = 0.1; PReset = 0.1; PShift = 0.0; PNew = 0.8 }
                    Shift = { PUnison = 0.2; PStepUp = 0.4; PStepDown = 0.4; PLeapUp = 0.0; PLeapDown = 0.0; MaxLeap = 0 }
                    MeterCreation = New (4, [ (4, 1.0) ], Duration.Half, 12 |> Pitch.make |> Note)
                    Meter = { PNote = 1.0; PHold = 0.0; PRest = 0.0 }
                    PhraseRepeat =
                        [ (4,
                            { PRepeat = 0.1; PReset = 0.1; PShift = 0.0; PNew = 0.8 },
                            { PUnison = 0.2; PStepUp = 0.4; PStepDown = 0.4; PLeapUp = 0.0; PLeapDown = 0.0; MaxLeap = 0 }) ] } ] }
          { Name = "Snare Drum"
            Instrument = Percussion
            Scale = PercussionInstrumentClass.Snare |> PercussionInstrumentClass.toInstrumentList |> List.map (int >> Pitch.make) |> List.toArray
            TrackOptions =
                [ { PSelectOption = 1.0
                    Modify = { PRepeat = 0.1; PReset = 0.1; PShift = 0.0; PNew = 0.8 }
                    Shift = { PUnison = 0.2; PStepUp = 0.4; PStepDown = 0.4; PLeapUp = 0.0; PLeapDown = 0.0; MaxLeap = 0 }
                    MeterCreation = RelativeTo ("Bass Drum", 2)
                    Meter = { PNote = 0.7; PHold = 0.0; PRest = 0.3 }
                    PhraseRepeat =
                        [ (2,
                            { PRepeat = 0.1; PReset = 0.1; PShift = 0.0; PNew = 0.8 },
                            { PUnison = 0.2; PStepUp = 0.4; PStepDown = 0.4; PLeapUp = 0.0; PLeapDown = 0.0; MaxLeap = 0 }) ] } ] }
          { Name = "Ride Cymbal"
            Instrument = Percussion
            Scale =
                [ OpenHihat; PedalHihat; RideCymbal1; RideCymbal2 ]
                |> List.map (int >> Pitch.make) |> List.toArray
            TrackOptions =
                [ { PSelectOption = 0.4
                    Modify = { PRepeat = 0.1; PReset = 0.1; PShift = 0.0; PNew = 0.8 }
                    Shift = { PUnison = 0.2; PStepUp = 0.4; PStepDown = 0.4; PLeapUp = 0.0; PLeapDown = 0.0; MaxLeap = 0 }
                    MeterCreation = New (4, [ (1, 1.0) ], Duration.Eighth, 12 |> Pitch.make |> Note)
                    Meter = { PNote = 1.0; PHold = 0.0; PRest = 0.0 }
                    PhraseRepeat = [] }
                  { PSelectOption = 0.6
                    Modify = { PRepeat = 0.1; PReset = 0.1; PShift = 0.0; PNew = 0.8 }
                    Shift = { PUnison = 0.2; PStepUp = 0.4; PStepDown = 0.4; PLeapUp = 0.0; PLeapDown = 0.0; MaxLeap = 0 }
                    MeterCreation = New (4, [ (1, 1.0) ], Duration.Eighth, Rest)
                    Meter = { PNote = 0.0; PHold = 0.0; PRest = 1.0 }
                    PhraseRepeat = [] } ] } ]
//          { Name = "Fill"
//            Instrument = Percussion
//            Scale =
//                [ CrashCymbal1; CrashCymbal2; HighTom1; HighTom2; MidTom1; MidTom2 ]
//                |> List.map int |> List.toArray |> Scale.make
//            TrackOptions =
//                [ { PSelectOption = 1.0
//                    Modify = { PRepeat = 0.05; PReset = 0.05; PShift = 0.0; PNew = 0.9 }
//                    Shift = { PUnison = 0.2; PStepUp = 0.4; PStepDown = 0.4; PLeapUp = 0.0; PLeapDown = 0.0; MaxLeap = 0 }
//                    MeterCreation = RelativeTo ("Ride Cymbal", 1)
//                    Meter = { PNote = 1.0; PHold = 0.0; PRest = 0.0 }
//                    PhraseRepeat = [] } ] } ]
open Parameters

module Debug =
    let debugn = Diagnostics.Debug.WriteLine
    let debug = Diagnostics.Debug.Write
    let printNotes list =
        Seq.iter
            (function
                | Note n -> n |> NoteOctave.ofPitch |> sprintf "%O " |> debug
                | Hold -> debug " ~~ "
                | Rest -> debug " XX ")
            list
        debugn ""
open Debug

[<EntryPoint>]
let main _ =
    debugn <| sprintf "seed = %i" seed
    let rng = Random seed

    let addNotes notes channel track =
        notes
        |> Seq.toList
        |> List.foldBack
            (fun (note, duration) (track, holdCount) ->
                match note with
                | Note pitch -> ((int pitch, holdCount + duration) :: track, 0)
                | Hold -> (track, holdCount + duration)
                | Rest -> ((-1, holdCount + duration) :: track, 0))
            <| ([], 0)
        |> fst
        |> List.fold (fun track (pitch, length) -> Track.addNote channel pitch length track) track

    let takeLength notes length =
        let total = ref 0
        Seq.takeWhile
            (fun (_, duration) ->
                total := !total + duration
                !total <= length)
            notes
        |> Seq.cache

    let createTrack name tempo instrument notes =
        let phrase = takeLength notes length
        printNotes (phrase |> Seq.map fst)
        let channel =
            if instrument = Percussion then 9 else 0
            |> Channel.make
        let instrument =
            match instrument with
            | Chromatic chromatic -> chromatic
            | _ -> AcousticGrandPiano
        Track.make name
        |> Track.setTimeSignature 4 4
        |> Track.setTempo (Tempo.make tempo)
        |> Track.setChannelInstrument channel instrument
        |> addNotes phrase channel

    let song =
        Conductor.conduct rng instruments
        |> Seq.zip (instruments |> List.map (fun param -> (param.Name, param.Instrument)))
        |> Seq.fold
            (fun song ((name, instrument), notes) ->
                debugn name
                match instrument with
                | Silent -> song
                | _ ->
                    createTrack name 90 instrument notes
                    |> Song.addTrack <| song)
            (Song.init 12)

    let filePath = IO.Path.Combine (Environment.SpecialFolder.Desktop |> Environment.GetFolderPath, "miditest.mid")
    use fileStream = new IO.FileStream (filePath, IO.FileMode.Create)
    Song.write fileStream song
    fileStream.Flush ()

    0
