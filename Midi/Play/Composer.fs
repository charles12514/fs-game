module Composer

open Midi
open Structure
open Structure.Parameters
open Util

[<NoComparison>]
type private Context =
  { Scale : Pitch array
    FirstPhrase : Phrase
    LastPhrase : Phrase }

module private Shift =
    type private Shift =
        | Unison
        | StepUp
        | StepDown
        | LeapUp of int
        | LeapDown of int

    let private leapAmt rng maxLeap =
        [ 2 .. maxLeap ]
        |> Seq.weightedRandom rng (fun amt -> 1.0 / float amt)

    let private toInt =
        function
        | Unison -> 0
        | StepUp -> 1
        | StepDown -> -1
        | LeapUp x -> x
        | LeapDown x -> -x

    let getRandom rng param =
        [ (Unison, param.PUnison)
          (StepUp, param.PStepUp)
          (StepDown, param.PStepDown)
          (LeapUp 0, param.PLeapUp)
          (LeapDown 0, param.PLeapDown) ]
        |> Seq.weightedRandom rng snd
        |> fst
        |> function
            | LeapUp _ -> leapAmt rng param.MaxLeap |> LeapUp
            | LeapDown _ -> leapAmt rng param.MaxLeap |> LeapDown
            | shift -> shift
        |> toInt

module private Mods =
    let repeat _ _ context = context.LastPhrase
    let reset _ _ context = context.FirstPhrase
    let shifter startIndex endIndex rng shiftParams context =
        Phrase.shiftRange startIndex endIndex (Shift.getRandom rng shiftParams) context.Scale context.LastPhrase
    let shiftOne (rng:System.Random) param context =
        let index =
            match context.LastPhrase with
            | Single _ -> 0
            | Phrase phrases -> rng.Next phrases.Length
        shifter index index rng param.Shift context
    let shiftStart (rng:System.Random) param context =
        let endIndex =
            match context.LastPhrase with
            | Single _ -> 0
            | Phrase phrases -> rng.Next (phrases.Length - 1)
        shifter 0 endIndex rng param.Shift context
    let shiftEnd (rng:System.Random) param context =
        let startIndex, endIndex =
            match context.LastPhrase with
            | Single _ -> 0, 0
            | Phrase phrases -> rng.Next (phrases.Length - 1) + 1, phrases.Length - 1
        shifter startIndex endIndex rng param.Shift context
    let shiftAll rng param context =
        let endIndex =
            match context.LastPhrase with
            | Single _ -> 0
            | Phrase phrases -> phrases.Length - 1
        shifter 0 endIndex rng param.Shift context
    let modify createNew rng param context =
        let pShiftD4 = param.Modify.PShift / 4.0
        [ (repeat, param.Modify.PRepeat)
          (reset, param.Modify.PReset)
          (shiftOne, pShiftD4)
          (shiftStart, pShiftD4)
          (shiftEnd, pShiftD4)
          (shiftAll, pShiftD4)
          (createNew, param.Modify.PNew) ]
        |> Seq.weightedRandom rng snd
        |> fst
        <||| (rng, param, context)
    let randomNote (rng:System.Random) _ context =
        let length = Array.length context.Scale
        let index =
            rng.NextNormal (float length / 2.0, float length / 6.0)
            |> System.Math.Round |> int
            |> max 0 |> min (length - 1)
        context.Scale.[index] |> Note |> Single
    let createFromMeter rng param scale meter =
        meter
        |> List.fold
            (fun (prev, context) ->
                function
                | Note _ ->
                    match context with
                    | Some context ->
                        let nextPhrase = modify randomNote rng param context
                        (nextPhrase :: prev, Some { context with LastPhrase = nextPhrase })
                    | None ->
                        let context = { Scale = scale; FirstPhrase = Single Rest; LastPhrase = Single Rest }
                        let note = randomNote rng param context
                        (note :: prev, Some { context with FirstPhrase = note; LastPhrase = note })
                | Hold -> (Single Hold :: prev, context)
                | Rest -> (Single Rest :: prev, context))
            ([], None)
        |> fst
        |> List.rev
        |> Phrase
    let rec createNew rng param context =
        let createN basePhrase n =
            if n <= 1 then basePhrase
            else
                let context = { context with FirstPhrase = basePhrase; LastPhrase = basePhrase }
                [ 1 .. n - 1 ]
                |> Seq.fold
                    (fun (phrases, context) _ ->
                        let nextPhrase = modify createNew rng param context
                        (nextPhrase :: phrases, { context with LastPhrase = nextPhrase }))
                    ([ basePhrase ], context)
                |> fst
                |> List.rev
                |> Phrase
        let rec create sample =
            match sample with
            | Single (Note _) -> randomNote rng param context
            | Single Hold -> Single Hold
            | Single Rest -> Single Rest
            | Phrase phrase ->
                if phrase = [] then Phrase []
                elif List.forall (function | Single _ -> true | Phrase _ -> false) phrase then
                    sample
                    |> Phrase.toList
                    |> createFromMeter rng param context.Scale
                else
                    phrase
                    |> List.head
                    |> create
                    |> createN <| phrase.Length
        create context.FirstPhrase
open Mods

module private Meter =
    // http://onloop.net/tenor
    let create rng pGroups beats =
        let rec pickGroups beats complete =
            if beats = 0 then List.rev complete
            else
                match pGroups |> List.filter (fun (n, _) -> n <= beats) with
                | [] -> pickGroups 0 (beats :: complete)
                | available ->
                    let selected = Seq.weightedRandom rng snd available |> fst
                    pickGroups (beats - selected) (selected :: complete)
        pickGroups beats []
    let private getRandomNote rng param =
        [ (12 |> Pitch.make |> Note, param.PNote); (Hold, param.PHold); (Rest, param.PRest) ]
        |> Seq.weightedRandom rng snd
        |> fst
    let expand rng param downbeat =
        List.collect (fun m -> downbeat :: [ for _ in 2 .. m -> getRandomNote rng param ])
    let expandPhrase rng param noteValue =
        List.collect (fun p -> p :: [ for _ in 2 .. noteValue -> getRandomNote rng param ])
    let rec fromPhrase =
        function
        | Phrase subphrases ->
            match subphrases with
            | Single _ :: _ -> Phrase subphrases |> Phrase.toList
            | Phrase head :: _ -> Phrase head |> fromPhrase
            | [] -> []
        | Single note -> [ note ]

let private modifyLoop rng param scale n phrase =
    let rec loop n context phrases =
        if n <= 1 then phrases |> List.rev |> Phrase
        else
            let next = modify createNew rng param context
            loop (n - 1) { context with LastPhrase = next } (next :: phrases)
    loop n { Scale = scale; FirstPhrase = phrase; LastPhrase = phrase } [ phrase ]

let rec private repeatPhrase rng param scale phrase =
    match param.PhraseRepeat with
    | [] -> phrase
    | (n, modifyParams, shiftParams) :: nextRepeat ->
        let param = { param with Modify = modifyParams; Shift = shiftParams; PhraseRepeat = nextRepeat }
        modifyLoop rng param scale n phrase
        |> repeatPhrase rng param scale

// Compose a phrase for a single instrument
let compose rng param meters =
    let trackOption = Seq.weightedRandom rng (fun t -> t.PSelectOption) param.TrackOptions
    let (meter, duration) =
        match trackOption.MeterCreation with
        | New (beats, pGroups, duration, downbeat) ->
            (Meter.create rng pGroups beats |> Meter.expand rng trackOption.Meter downbeat, int duration)
        | RelativeTo (baseTrack, expandTo) ->
            match Map.tryFind baseTrack meters with
            | None -> failwithf "Error creating meter for track %s: no base track with name %s" param.Name baseTrack
            | Some (basePhrase, baseDuration) ->
                if baseDuration % expandTo <> 0 then failwithf "Error creating meter for track %s: base duration of %i is not evenly divisible by expand factor %i" param.Name baseDuration expandTo
                (basePhrase |> Meter.fromPhrase |> Meter.expandPhrase rng trackOption.Meter expandTo, baseDuration / expandTo)
    (meter |> createFromMeter rng trackOption param.Scale |> repeatPhrase rng trackOption param.Scale, duration)
    