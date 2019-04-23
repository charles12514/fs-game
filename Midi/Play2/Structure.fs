module Structure

open Midi
open Util

type Note =
    | Note of Pitch
    | Hold
    | Rest
    
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Note =
    let map f =
        function
        | Note n -> n |> f |> Note
        | Hold -> Hold
        | Rest -> Rest

    let shift intervals =
        map ((+) intervals)

type Phrase =
    | Single of Note
    | Phrase of Phrase list

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Phrase =
    let rec flatten =
        function
        | Single note -> Seq.singleton note
        | Phrase phrase -> Seq.collect flatten phrase

    let rec shift intervals =
        function
        | Single note ->
            note
            |> Note.shift intervals
            |> Single
        | Phrase subphrases ->
            subphrases
            |> List.map (shift intervals)
            |> Phrase

    let rec tryFirst = flatten >> Seq.tryHead
    let rec tryLast = flatten >> Seq.tryLast

type Command =
    | Literal of Phrase
    | Clear
    | Compound
    | Repeat
    | Reset
    | Shift of int
    | ShiftFirst of int
    | ShiftLast of int

module Shift =
    type Params =
      { PUnison : float
        PStepUp : float
        PStepDown : float
        PLeapUp : float
        PLeapDown : float
        MaxLeap : int }

    type private Shift =
        | Unison
        | StepUp
        | StepDown
        | LeapUp of int
        | LeapDown of int

    /// Get a random pitch leap distance. Return value will be between 2 and maxLeap, inclusive. Smaller leaps are more likely.
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

    /// Get a weighted random shift distance.
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

module Meter =
    type Params =
      { BeatCount : int
        BeatGroupings : (int * float) list
        Downbeat : Note
        PNote : float
        PHold : float
        PRest : float
        Shift : Shift.Params }

    /// Get a random value from [ Note 0; Hold; Rest ], weighted by parameters
    let private getRandomNote rng meterParams =
        [ (0 |> Pitch.make |> Note, meterParams.PNote); (Hold, meterParams.PHold); (Rest, meterParams.PRest) ]
        |> Seq.weightedRandom rng snd
        |> fst

    /// Create a random meter. All Notes will have value 0.
    let private createPlaceholder rng meterParams =
        let rec divideGroups beats completed =
            if beats = 0 then List.rev completed
            else
                match meterParams.BeatGroupings |> List.filter (fun (n, _) -> n > 0 && n <= beats) with
                | [] -> divideGroups 0 (beats :: completed)
                | available ->
                    let selected = Seq.weightedRandom rng snd available |> fst
                    divideGroups (beats - selected) (selected :: completed)
        divideGroups meterParams.BeatCount []
        |> List.collect
            (fun groupSize ->
                meterParams.Downbeat
                :: List.init
                    (groupSize - 1)
                    (fun _ -> getRandomNote rng meterParams))
        |> List.map (Note.map (fun _ -> Pitch.make 0))

    /// Add pitch variation to an existing meter. The first Note will have pitch 0, the remaining notes will be shifted. Pitches may become negative.
    let private addVariation rng shiftParams =
        List.fold
            (fun (lastNote, completed) note ->
                let revisedNote =
                    note
                    |> Note.map
                        (fun n ->
                            match lastNote with
                            | Some last -> int last + Shift.getRandom rng shiftParams |> Pitch.make
                            | None -> n)
                let lastNote =
                    match revisedNote with
                    | Note n -> Some n
                    | _ -> lastNote
                (lastNote, revisedNote :: completed))
            (None, [])
        >> snd
        >> List.rev

    /// Create a random, pitched meter, starting at pitch 0. Pitches may be negative.
    let create rng meterParams =
        createPlaceholder rng meterParams
        |> addVariation rng meterParams.Shift
        |> List.map Single
        |> Phrase

module Compounder =
    type Params =
      { PRepeat : float
        PReset : float
        PShift : float
        PShiftFirst : float
        PShiftLast : float
        PNew : float
        Shift : Shift.Params
        Repeat : int
        Compound : Params option }

    type private Cmd =
        | DoRepeat
        | DoReset
        | DoShift
        | DoShiftFirst
        | DoShiftLast
        | DoNew

    /// Create a finite, cached sequence of commands.
    let compound rng meterParams compounderParams =
        seq {
            yield meterParams |> Meter.create rng |> Literal
            yield!
                Seq.unfold
                    (fun trackParams ->
                        trackParams
                        |> Option.bind
                            (fun trackParams ->
                                if trackParams.Repeat > 0 then
                                    [ (DoRepeat, trackParams.PRepeat)
                                      (DoReset, trackParams.PReset)
                                      (DoShift, trackParams.PShift)
                                      (DoNew, trackParams.PNew) ]
                                    |> Seq.weightedRandom rng snd
                                    |> fst
                                    |> function
                                        | DoRepeat -> Repeat
                                        | DoReset -> Reset
                                        | DoShift -> Shift.getRandom rng trackParams.Shift |> Shift
                                        | DoShiftFirst -> Shift.getRandom rng trackParams.Shift |> ShiftFirst
                                        | DoShiftLast -> Shift.getRandom rng trackParams.Shift |> ShiftLast
                                        | DoNew ->
                                            meterParams
                                            |> Meter.create rng
                                            |> Literal
                                    |> fun cmd ->
                                        (cmd, Some { trackParams with Repeat = trackParams.Repeat - 1 })
                                        |> Some
                                else
                                    match trackParams.Compound with
                                    | Some compoundParams -> (Compound, Some compoundParams)
                                    | None -> (Clear, None)
                                    |> Some))
                    (Some compounderParams)
        }
        |> Seq.cache

module TrackComponent =
    type Params =
      { Meter : Meter.Params
        Compounder : Compounder.Params
        PChoose : float }

    /// Create a finite, cached sequence of commands.
    let expand rng componentParams =
        Compounder.compound rng componentParams.Meter componentParams.Compounder

module Track =
    type Params =
      { Components : TrackComponent.Params list }

    /// Create an infinite, cached sequence of commands. Notes will be centered around 0 and may be negative.
    let createCommands rng trackParams =
        Seq.initInfinite
            (fun _ ->
                trackParams.Components
                |> Seq.weightedRandom rng (fun p -> p.PChoose)
                |> TrackComponent.expand rng)
        |> Seq.collect id
        |> Seq.cache
        
module Elaborate =
    let private mapNextFromHead f =
            function
            | (h :: _) as list ->
                let mapped = f h
                (mapped :: list, Some mapped)
            | [] -> ([], None)

    /// Apply a command to a given phrase builder.
    /// Returns the phrase builder after the command is applied, and any new notes that would be output from the command.
    /// The phrase builder is expected and returned in reversed order.
    let applyCommand phraseBuilder =
        function
            | Literal note ->
                (note :: phraseBuilder, Some note)
            | Clear ->
                ([ Phrase [] ], None)
            | Compound ->
                let currentPhrase =
                    phraseBuilder
                    |> List.rev
                    |> Phrase
                ([ currentPhrase ], None)
            | Repeat ->
                phraseBuilder
                |> mapNextFromHead id
            | Reset ->
                match List.tryLast phraseBuilder with
                | Some firstPhrase -> (firstPhrase :: phraseBuilder, Some firstPhrase)
                | None -> ([], None)
            | Shift shift ->
                phraseBuilder
                |> mapNextFromHead (Phrase.shift shift)
            | ShiftFirst shift ->
                phraseBuilder
                |> mapNextFromHead
                    (function
                        | Single single -> single |> Note.shift shift |> Single
                        | Phrase phrases ->
                            match phrases with
                            | head :: tail ->
                                Phrase.shift shift head :: tail
                                |> Phrase
                            | [] -> Phrase [])
            | ShiftLast shift ->
                phraseBuilder
                |> mapNextFromHead
                    (function
                        | Single single -> single |> Note.shift shift |> Single
                        | Phrase phrases ->
                            phrases
                            |> List.mapi
                                (fun index phrase ->
                                    if index = phrases.Length - 1 then Phrase.shift shift phrase
                                    else phrase)
                            |> Phrase)

    /// Translate commands to notes.
    let toNotes =
        let folder (phraseBuilder, _) = applyCommand phraseBuilder
        fun commands ->
            commands
            |> Seq.scan folder ([], None)
            |> Seq.choose snd
            |> Seq.collect Phrase.flatten
            |> Seq.cache
           
let sequentialToScale scale =
    Seq.map
        (Note.map
            (fun n ->
                n
                |> int
                |> max 0
                |> min (Array.length scale - 1)
                |> Array.get scale))

let write seed scale =
    Track.createCommands (System.Random seed)
    >> Elaborate.toNotes
    >> Seq.map (Note.shift <| (Array.length scale - 1) / 2)
    >> sequentialToScale scale
