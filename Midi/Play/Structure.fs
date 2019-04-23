module Structure

open Midi

type System.Random with
    member this.NextNormal () =
        let u1 = this.NextDouble ()
        let u2 = this.NextDouble ()
        let r = u1 |> log |> (*) -2.0 |> sqrt
        let theta = 2.0 * System.Math.PI * u2
        r * sin theta
    member this.NextNormal (mean, standardDeviation) = this.NextNormal () * standardDeviation + mean

type Note =
    | Note of Pitch
    | Hold
    | Rest

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Note =
    let shift intervals scale =
        function
        | Note n ->
            let index =
                let i = scale |> Array.findIndex ((=) n) |> (+) intervals
                if i < 0 then 0
                elif i >= Array.length scale then Array.length scale - 1
                else i
            scale.[index] |> Note
        | Hold -> Hold
        | Rest -> Rest

type Phrase =
    | Single of Note
    | Phrase of Phrase list

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Phrase =
    let rec private shift intervals scale =
        function
        | Single note ->
            note
            |> Note.shift intervals scale
            |> Single
        | Phrase subphrases ->
            subphrases
            |> List.map (shift intervals scale)
            |> Phrase

    let shiftRange startIndex endIndex intervals scale phrase =
        match phrase with
        | Single _ -> shift intervals scale phrase
        | Phrase subphrases ->
            subphrases
            |> List.mapi (fun index p -> if index >= startIndex && index <= endIndex then shift intervals scale p else p)
            |> Phrase

    let rec toList =
        function
        | Single note -> [ note ]
        | Phrase phrase -> List.collect toList phrase

    let rec toSeq phrase =
        seq {
            match phrase with
            | Single note -> yield note
            | Phrase phrase -> yield! Seq.collect toSeq phrase }

module Parameters =
    type Instrument =
        | Chromatic of Midi.ChromaticInstrument
        | Percussion
        | Silent

    type ModifyParams =
      { PRepeat : float
        PReset : float
        PShift : float
        PNew : float }

    type ShiftParams =
      { PUnison : float
        PStepUp : float
        PStepDown : float
        PLeapUp : float
        PLeapDown : float
        MaxLeap : int }

    type MeterCreation =
        | New of int * (int * float) list * Midi.Duration * Note
        | RelativeTo of string * int

    type MeterParams =
      { PNote : float
        PHold : float
        PRest : float }

    type TrackOptionParams =
      { PSelectOption : float
        MeterCreation : MeterCreation
        Meter : MeterParams
        Modify : ModifyParams
        Shift : ShiftParams
        PhraseRepeat : (int * ModifyParams * ShiftParams) list }

    [<NoComparison>]
    type Params =
      { Name : string
        Instrument : Instrument
        Scale : Pitch array
        TrackOptions : TrackOptionParams list }
