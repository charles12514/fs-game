namespace Midi

type Octave =
    private { Octave : int }
    static member op_Explicit octave = octave.Octave

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Octave =
    let MinValue = -1
    let MaxValue = 9
    let length = 12
    let make octave =
        { Octave = octave |> max MinValue |> min MaxValue }

type Note =
    | C
    | ``C#`` | Db
    | D
    | ``D#`` | Eb
    | E
    | F
    | ``F#`` | Gb
    | G
    | ``G#`` | Ab
    | A
    | ``A#`` | Bb
    | B

    static member op_Explicit note =
        match note with
        | C -> 0
        | ``C#`` | Db -> 1
        | D -> 2
        | ``D#`` | Eb -> 3
        | E -> 4
        | F -> 5
        | ``F#`` | Gb -> 6
        | G -> 7
        | ``G#`` | Ab -> 8
        | A -> 9
        | ``A#`` | Bb -> 10
        | B -> 11
    static member ofInt note =
        let note = ((note % Octave.length) + Octave.length) % Octave.length
        match note with
        | 0 -> C
        | 1 -> ``C#``
        | 2 -> D
        | 3 -> ``D#``
        | 4 -> E
        | 5 -> F
        | 6 -> ``F#``
        | 7 -> G
        | 8 -> ``G#``
        | 9 -> A
        | 10 -> ``A#``
        | 11 -> B
        | _abandonAllHope -> C
    static member (+) (note:Note, intervals:int) =
        note |> int |> (+) intervals |> Note.ofInt
    override this.ToString () =
        match this with
        | C -> "C"
        | ``C#`` -> "C#"
        | Db -> "Db"
        | D -> "D"
        | ``D#`` -> "D#"
        | Eb -> "Eb"
        | E -> "E"
        | F -> "F"
        | ``F#`` -> "F#"
        | Gb -> "Gb"
        | G -> "G"
        | ``G#`` -> "G#"
        | Ab -> "Ab"
        | A -> "A"
        | ``A#`` -> "A#"
        | Bb -> "Bb"
        | B -> "B"

type Pitch =
    private { Pitch : int }
    static member op_Explicit pitch = pitch.Pitch

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Pitch =
    let MinValue = 0
    let MaxValue = 127
    let make pitch =
        { Pitch = pitch |> max MinValue |> min MaxValue }

type Pitch with
    static member (+) (pitch:Pitch, n:int) = int pitch + n |> Pitch.make
    static member (+) (n:int, pitch:Pitch) = pitch + n
    static member (+) (p1:Pitch, p2:Pitch) = p1 + int p2

type NoteOctave =
  { Note : Note
    Octave : Octave }

    override this.ToString () =
        sprintf "%O%i" this.Note <| int this.Octave

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NoteOctave =
    let tryPitch { Note = note; Octave = octave } =
        let p = (int octave + 1) % Octave.length + int note
        if p >= Pitch.MinValue && p <= Pitch.MaxValue then
            p |> Pitch.make |> Some
        else None
    let toPitch { Note = note; Octave = octave } =
        (int octave + 1) * Octave.length + int note
        |> Pitch.make
    let ofPitch (pitch:Pitch) =
        { Note = int pitch % Octave.length |> Note.ofInt
          Octave = int pitch / Octave.length - 1 |> Octave.make }
