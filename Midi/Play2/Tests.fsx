#r @"C:\Users\csnyder\Desktop\fs-game\packages\FsCheck.2.6.0\lib\net45\FsCheck.dll"
#r @"C:\Users\csnyder\Desktop\fs-game\Util\bin\Debug\Util.dll"
#r @"C:\Users\csnyder\Desktop\fs-game\Midi\Midi\bin\Debug\Midi.dll"

#load "Structure.fs"
#load "Reverse.fs"
#load "Transform.fs"

open FsCheck
open Structure
open Transform

let isEquivalent a b =
    let rec f a b restA restB =
        match (a, b) with
        | ([], []) -> true
        | (a1 :: a2, b1 :: b2) ->
            match (a1, b1) with
            | (Note n1, Note n2) when n1 = n2 -> f a2 b2 false false
            | (Rest, Rest) -> f a2 b2 true true
            | (Hold, Hold) -> f a2 b2 restA restB
            | (Rest, Hold) ->
                if restB then f a2 b2 true true
                else false
            | (Hold, Rest) ->
                if restA then f a2 b2 true true
                else false
            | _ -> false
        | _ -> false
    f a b false false

type Tests =
    static member ``toCommands >> toNotes ~= id`` notes =
        notes
        |> Reverse.toCommands
        |> Elaborate.toNotes
        |> Seq.toList
        |> (=) notes
    static member ``noteListToData >> dataToNoteList ~= id`` notes =
        let notes =
            match notes with
            | Hold :: tail -> Rest :: tail
            | notes -> notes
        if notes |> List.exists (function | Note _ -> true | _ -> false) then
            let min = // noteListToData will shift all notes down by min
                notes
                |> List.choose
                    (function
                        | Note n -> Some n
                        | Hold | Rest -> None)
                |> List.min
            notes
            |> Transforms.noteListToData
            |> Transforms.dataToNoteList
            |> List.map (Note.shift min)
            |> isEquivalent notes
        else true

Check.QuickAll<Tests> ()
