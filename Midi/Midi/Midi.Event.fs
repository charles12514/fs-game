namespace Midi

open System.IO
open Util

type Tempo =
    private { Tempo : int }

    static member op_Explicit tempo = tempo.Tempo

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Tempo =
    let make tempo =
        if tempo >= 1 && tempo <= 60000000 then
            { Tempo = tempo }
        else
            failwithf "Invalid tempo %i, must be between 1 and 60000000" tempo

type Channel =
    private { Channel : int }

    static member op_Explicit channel = channel.Channel
    static member op_Explicit channel = channel |> int |> byte

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Channel =
    let make channel =
        if channel >= 0x0 && channel <= 0xF then
            { Channel = channel }
        else
            failwithf "Invalid channel number %i, must be between 0x0 and 0xF" channel

/// <summary>
/// VariableLength is a static class providing functionality to write out standard MIDI Variable Length quantities.
/// Two methods are available in this class, allowing programmers to directly output variable length quantities
/// to a FileStream, or output them to a List of bytes.
/// </summary>
module VariableLength =
    /// <summary>
    /// This function accepts an unsigned long (ulong) input and outputs, directly to a FileStream,
    /// a standard MIDI variable-length equivalent.
    /// </summary>
    /// <param name="objWriter">A FileStream to which the standard MIDI variable-length quantity will be written.</param>
    /// <param name="value">The value to be written as a variable-length quantity. Must be cast to an unsigned long (ulong).</param>
    let writeToStream (objWriter:Stream) value =
        // for variable length quantities, determine the length of the output.
        // there can be a maximum of 4 bytes output.
        let iterations =
            if value < 0x80UL then 1
            elif value < 0x4000UL then 2
            elif value < 0x200000UL then 3
            else 4
        // calculate the variable length bytes, according to the MIDI specification.
        let rec f buffer value =
            if value <> 0UL then f ((buffer <<< 8) ||| ((value &&& 0x7FUL) ||| 0x80UL)) (value >>> 7)
            else buffer
        let rec g iter buffer =
            if iter = 0 then ()
            else
                objWriter.WriteByte (byte (buffer &&& 0xFFUL))
                g (iter - 1) (buffer >>> 8)
        f (value &&& 0x7FUL) (value >>> 7)
        |> g iterations

    /// <summary>
    /// This function accepts an unsigned long (ulong) input and outputs, directly to a List of bytes,
    /// a standard MIDI variable-length equivalent. The functionality is equivalent to the alternate WriteVariableLength() function.
    /// </summary>
    /// <param name="objWriter">A List of bytes to which the standard MIDI variable-length quantity will be written.</param>
    /// <param name="value">The value to be written as a variable-length quantity. Must be cast to an unsigned long (ulong).</param>
    let toBytes value =
        // for variable length quantities, determine the length of the output.
        // there can be a maximum of 4 bytes output.
        let iterations =
            if value < 0x80UL then 1
            elif value < 0x4000UL then 2
            elif value < 0x200000UL then 3
            else 4
        // calculate the variable length bytes, according to the MIDI specification.
        let rec f buffer value =
            if value <> 0UL then f ((buffer <<< 8) ||| ((value &&& 0x7FUL) ||| 0x80UL)) (value >>> 7)
            else buffer
        let rec g iter list buffer =
            if iter = 0 then list |> List.rev
            else g (iter - 1) (byte (buffer &&& 0xFFUL) :: list) (buffer >>> 8)
        f (value &&& 0x7FUL) (value >>> 7)
        |> g iterations []

    let tryParse =
        let rec read acc =
            function
            | h :: t ->
                let acc = (acc <<< 7) ||| (h &&& 0x7Fuy |> uint64)
                if h < 0x80uy then Some (acc, t)
                else read acc t
            | [] -> None
        read 0UL

type Event =
    | TrackHeader of string
    | SetChannelInstrument of Channel * ChromaticInstrument
    | SetTempo of Tempo
    | SetTimeSignature of int * int
    | NoteOff of Channel * int * byte
    | NoteOn of Channel * int * byte
    | Aftertouch of Channel * int * byte * byte // note, pressure, offset
    | ControlChange of Channel * byte * byte * byte // controller number, controller value, offset
    | ProgramChange of Channel * byte * byte // program number, offset

[<RequireQualifiedAccess>]
module ControllerNumber =
    let bankSelect = 0uy
    let modulationWheel = 1uy
    let breathController = 2uy
    let footController = 4uy
    let portamentoTime = 5uy
    let dataEntry = 6uy
    let channelVolume = 7uy
    let balance = 8uy
    let pan = 10uy
    let expressionController = 11uy
    let effectControl1 = 12uy
    let effectControl2 = 13uy
    let damperPedal = 64uy // value <= 63 is off, >= 64 is on
    let portamento = 65uy // value <= 63 is off, >= 64 is on // slide
    let sostenuto = 66uy // value <= 63 is off, >= 64 is on // sustain
    let softPedal = 67uy // value <= 63 is off, >= 64 is on
    let legatoFootswitch = 68uy // value <= 63 is normal, >= 64 is legato
    let hold2 = 69uy // value <= 63 is off, >= 64 is on

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Event =
    let toBytes =
        function
        | TrackHeader name ->
            [ 0x00uy; 0xFFuy; 0x03uy ]                          // populate the header event with the standard MIDI track header
            @ VariableLength.toBytes (name.Length |> uint64)    // Write out the length of the track's human-readable name (as a string of characters)
            @ (name |> Seq.map byte |> Seq.toList)              // Write each character in the track's human-readable name out as an ASCII byte
        | SetChannelInstrument (channel, instrument) ->
            // If not specified, grand piano (1) is default
            [ 0x00uy                        // the MIDI instrument (program change) command is defined as a MIDI-event (no need for a leading 0xFF)
              0xC0uy ||| byte channel       // It is defined as 0xCX where X is the channel number (0x0 - 0xF) to which the given instrument will be tied
              instrument |> int |> byte ]   // the MIDI standard instrument number
        | SetTempo tempo ->
            // If not specified, 100 is default
            // as per the MIDI specification, tempo is stored in the number of microseconds per quarter note.
            // this function takes in a BeatsPerMinute (int Tempo). To find the number of microseconds per quarter note,
            // 60,000,000 must be divided by the number of beats per minute (one quarter note is defined as one beat).
            let microsecondsPerQuarterNote = 60000000 / int tempo
            [ 0x00uy    // The Tempo command is a non-MIDI event (starting with 0xFF)
              0xFFuy
              0x51uy    // The Tempo command is defined as command 0x51
              0x03uy    // The 0x03 declares that three bytes of data follow
              // the 3 bytes of data are defined to be the quantity (microsecondsPerQuarterNote (see above)), staring with the big-endian most significant byte (0xFF 00 00)
              (0xFF0000 &&& microsecondsPerQuarterNote) >>> 16 |> byte
              (0xFF00 &&& microsecondsPerQuarterNote) >>> 8 |> byte
              0xFF &&& microsecondsPerQuarterNote |> byte ]
        | SetTimeSignature (numerator, denominator) ->
            [ 0x00uy    // time signature command is a non-MIDI event (starting with 0xFF)
              0xFFuy
              0x58uy    // it is type 0x58
              0x04uy    // The 0x04 defines 4 bytes following the command header. The four bytes are as follows:
              byte numerator    // numerator of time signature "fraction" (6 in 6/8)
              System.Math.Log (float denominator, 2.0) |> int |> byte   // The log base 2 of the denominator of the time signature fraction (log(2) of 8 in 6/8)
              0x18uy    // The number of ticks in a metronome click. By default, this is defined as 24 (0x18)
              0x08uy ]  // The number of 32nd note sin a quarter-note. By default, this is defined as 8 (0x08)
        | NoteOn (channel, midiNoteNumber, offset) ->
            VariableLength.toBytes (offset |> uint64)   // write out the duration of the note as a variable length quantity
            @
            [ 0x90uy ||| byte channel
              byte midiNoteNumber                       // write out the standard MIDI note number that will be played
              0x7Fuy ]                                  // velocity, defined as 0x7F (full)
        | NoteOff (channel, midiNoteNumber, offset) ->
            VariableLength.toBytes (offset |> uint64)   // write out the duration of the note as a variable length quantity
            @
            [ 0x80uy ||| byte channel
              byte midiNoteNumber                       // write out the standard MIDI note number that will be played
              0x7Fuy ]                                  // velocity, defined as 0x7F (full)
        | Aftertouch (channel, midiNoteNumber, pressure, offset) ->
            VariableLength.toBytes (offset |> uint64)
            @
            [ 0xA0uy ||| byte channel
              byte midiNoteNumber
              pressure ]
        | ControlChange (channel, controllerNumber, controllerValue, offset) ->
            VariableLength.toBytes (offset |> uint64)
            @
            [ 0xB0uy ||| byte channel
              controllerNumber
              controllerValue ]
        | ProgramChange (channel, programNumber, offset) ->
            VariableLength.toBytes (offset |> uint64)
            @
            [ 0xC0uy ||| byte channel
              programNumber ]
    let tryParse =
        function
        | 0x00uy :: 0xFFuy :: 0x03uy :: t ->
            match VariableLength.tryParse t with
            | Some x -> Success x
            | None -> Failure "Failed parsing length for track header"
            |> Attempt.bind
                (fun (length, t) ->
                    let length = int length
                    let name =
                        t
                        |> List.take length
                        |> List.map char
                        |> Array.ofList
                        |> System.String
                    let t = List.skip length t
                    Success (TrackHeader name, t))
        | 0x00uy :: ch :: instr :: t when ch &&& 0xF0uy = 0xC0uy ->
            Success (SetChannelInstrument (ch &&& 0xFuy |> int |> Channel.make, instr |> int |> ChromaticInstrument.ofInt), t)
        | 0x00uy :: 0xFFuy :: 0x51uy :: 0x03uy :: m1 :: m2 :: m3 :: t ->
            let microsecondsPerQuarterNote =
                (int m1 <<< 16)
                ||| (int m2 <<< 8)
                ||| (int m3)
            Success (SetTempo (60000000 / microsecondsPerQuarterNote |> Tempo.make), t)
        | 0x00uy :: 0xFFuy :: 0x58uy :: 0x04uy :: num :: den :: 0x18uy :: 0x08uy :: t ->
            Success (SetTimeSignature (int num, 2.0 ** (den |> float) |> int), t)
        | t ->
            let t' = t
            match VariableLength.tryParse t with
            | Some x -> Success x
            | None -> Failure "Failed parsing offset for event"
            |> Attempt.bind
                (fun (offset, t) ->
                    match t with
                    | ch :: note :: velocity :: t when ch &&& 0xF0uy = 0x80uy ->
                        ignore velocity
                        Success (NoteOff (ch &&& 0xFuy |> int |> Channel.make, int note, byte offset), t)
                    | ch :: note :: velocity :: t when ch &&& 0xF0uy = 0x90uy ->
                        ignore velocity
                        Success (NoteOn (ch &&& 0xFuy |> int |> Channel.make, int note, byte offset), t)
                    | ch :: note :: pressure :: t when ch &&& 0xF0uy = 0xA0uy ->
                        Success (Aftertouch (ch &&& 0xFuy |> int |> Channel.make, int note, pressure, byte offset), t)
                    | ch :: controllerNumber :: controllerValue :: t when ch &&& 0xF0uy = 0xB0uy ->
                        Success (ControlChange (ch &&& 0xFuy |> int |> Channel.make, controllerNumber, controllerValue, byte offset), t)
                    | ch :: programNumber :: t when ch &&& 0xF0uy = 0xC0uy ->
                        Success (ProgramChange (ch &&& 0xFuy |> int |> Channel.make, programNumber, byte offset), t)
                    | _ ->
                        let t = t |> List.map (fun b -> b.ToString("X"))
                        let t' = t' |> List.map (fun b -> b.ToString("X"))
                        sprintf "Unknown event, offset = %d, bytes : %A\n\norig bytes : %A" offset t t'
                        |> Failure)
