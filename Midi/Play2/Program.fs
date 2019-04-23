open Midi
open Structure
open Structure.Elaborate
open System.Diagnostics
open System.IO
open Util

let debug = Debug.Write
let debugn = Debug.WriteLine

let sampleCmds =
    [
        Literal (0 |> Pitch.make |> Note |> Single)
        Shift 1
        Shift -1
        Shift 2
        Compound
        Shift 2
        Compound
        ShiftLast 1
        Clear
        Literal (20 |> Pitch.make |> Note |> Single)
        Shift 2
        Compound
        Shift 1
    ]
let sampleResult =
    [ 0; 1; 0; 2; 2; 3; 2; 4; 0; 1; 0; 2; 3; 4; 3; 5; 20; 22; 21; 23 ]
    |> List.map (Pitch.make >> Note)
let testSample () =
    let result =
        sampleCmds
        |> toNotes
    result
    |> Seq.map
        (function
         | Note n -> n |> int |> sprintf "%i"
         | Rest -> "XX"
         | Hold -> "~~")
    |> Seq.iter (sprintf "%2s " >> debug)
    debugn ""
    result
    |> Seq.toList
    |> (=) sampleResult

[<EntryPoint>]
let main _ =
//    testSample ()
//    |> function
//        | true -> debugn "match"
//        | false -> debugn "fail"
//    debugn ""
//
//    sampleResult
//    |> Reverse.toCommands
//    |> Seq.iter (sprintf "%A" >> debugn)
//    debugn ""
    use file = new FileStream (@"C:\Users\csnyder\Desktop\Explosions in the Sky - Your Hand in Mine.mid", FileMode.Open)
    
    match Song.tryMake file with
    | Success song ->
        song
        |> Song.tracks
        |> List.iteri
            (fun i track ->
                sprintf "Track #%i" i |> debugn
                track
                |> Transform.Transforms.trackToData
                |> Transform.Transforms.dataToNoteList
                |> Seq.iter (sprintf "%A" >> debugn)
                debugn "")
    | Failure f -> sprintf "Failure: %s" f |> debugn

    0
