module Conductor

open Structure
open Structure.Parameters

module private Helpers =
    // If this instrument's phrase is over, return it and updated state with new phrase
    let tryRewrite rng (newPhrases, current) instrument =
        if Map.find instrument.Name current |> snd = 0 then
            let (phrase, duration) =
                current
                |> Map.map (fun _ cur -> fst cur)
                |> Composer.compose rng instrument
            let time = phrase |> Phrase.toSeq |> Seq.length |> (*) duration
            (Some (phrase, duration) :: newPhrases, current |> Map.add instrument.Name ((phrase, duration), time))
        else (None :: newPhrases, current)

    // Advance time until any currently playing phrase ends
    // Return phrases that ended, write new phrases for those instruments
    let advance rng instruments current =
        let min =
            current
            |> Map.toSeq
            |> Seq.map (snd >> snd)
            |> Seq.min
        let current = Map.map (fun _ (phraseAndDuration, time) -> (phraseAndDuration, time - min)) current
        instruments
        |> List.fold (tryRewrite rng) ([], current)
        |> fun (newPhrases, current) -> (List.rev newPhrases, current)

    let flattenPhrase (phrase, duration) =
        phrase
        |> Phrase.toSeq
        |> Seq.map (fun note -> (note, duration))
open Helpers

// Generate a list of infinite sequences of notes
let conduct rng instruments =
    instruments
    |> List.map (fun instrument -> (instrument.Name, ((Single Note.Rest, 0), 0)))
    |> Map.ofList
    |> Seq.unfold (advance rng instruments >> Some)
    |> Seq.cache
    |> fun s ->
        [ 0 .. instruments.Length - 1 ]
        |> List.map (fun i -> s |> Seq.map (fun timestep -> timestep.[i]))
    |> List.map (Seq.choose id >> Seq.collect flattenPhrase >> Seq.cache)
