namespace Midi

open Util

type Track =
    private
      { Name : string
        ReversedEvents : Event list }
    override this.ToString () =
        this.ReversedEvents |> List.rev |> sprintf "%A"

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Track =
    let name track = track.Name
    let events track = track.ReversedEvents |> List.rev
    let make name =
        { Name = name
          ReversedEvents = [ TrackHeader name ] }
    let toMetadata track =
        TrackStart (track.ReversedEvents |> List.sumBy (Event.toBytes >> List.length) |> uint64 |> (+) 4UL)
        :: (events track |> List.map Event)
        @ [ TrackEnd ]
    let fromEvents =
        function
        | TrackHeader name :: events ->
            if List.forall (function | TrackHeader _ -> false | _ -> true) events then
                { Name = name
                  ReversedEvents = TrackHeader name :: events |> List.rev }
                |> Success
            else Failure "Multiple track headers in one track"
        | events ->
            { Name = ""
              ReversedEvents = events |> List.rev }
            |> Success
    let addEvent event track =
        { track with ReversedEvents = event :: track.ReversedEvents }
    let setChannelInstrument channel instrument =
        SetChannelInstrument (channel, instrument)
        |> addEvent
    let setTempo tempo =
        SetTempo tempo
        |> addEvent
    let setTimeSignature numerator denominator =
        SetTimeSignature (numerator, denominator)
        |> addEvent
    let mapOffsets f track =
        { track with
            ReversedEvents =
                track.ReversedEvents
                |> List.map
                    (function
                     | TrackHeader _
                     | SetChannelInstrument _
                     | SetTempo _
                     | SetTimeSignature _ as noOffset -> noOffset
                     | NoteOff (channel, note, offset) -> NoteOff (channel, note, f offset)
                     | NoteOn (channel, note, offset) -> NoteOn (channel, note, f offset)
                     | Aftertouch (channel, note, pressure, offset) -> Aftertouch (channel, note, pressure, f offset)
                     | ControlChange (channel, number, value, offset) -> ControlChange (channel, number, value, f offset)
                     | ProgramChange (channel, number, offset) -> ProgramChange (channel, number, f offset) ) }

    /// <summary>
    /// AddNote(int, int, int, int) allows users to add playable notes to the MIDI file.
    /// </summary>
    /// <param name="channel">The track's channel (0 - 16) through which the note will be played.</param>
    /// <param name="midiNoteNumber">The MIDI note number (0 for low-C, etc.) that should be played. These note numbers are specified by the MIDI standard specification.
    /// -1: Rest
    /// </param>
    /// <param name="duration">The duration, in time ticks, that the note should be played before stopping. The timing is specified as below:
    /// 48: Whole Note
    /// 24: Half Note
    /// 12: Quarter Note
    /// 6: Eighth (1/8) Note
    /// 4: Tripolet Note
    /// 3: 1/16 Note
    /// </param>
    let addNote channel midiNoteNumber duration track =
        // It is useful to note here that the time of a MIDI file is cumulative. That is,
        // at any given point, an "offset" or time stamp of 0x00 means "presently" (the time when the last
        // event concluded.
        // Therefore, when adding a note, an initial time-stamp of 0x00 is used.
        // When stopping a note (ending a note), a duration is used as a time-stamp, representing the length of the note.

        // if the user entered a rest, start the note as being not-pressed.
        if midiNoteNumber = -1 then
            track
            |> addEvent (NoteOff (channel, 0, 0x00uy))
            |> addEvent (NoteOff (channel, 0, byte duration))
        // otherwise, start the note as being pressed.
        else
            track
            |> addEvent (NoteOn (channel, midiNoteNumber, 0x00uy))
            |> addEvent (NoteOff (channel, midiNoteNumber, byte duration))

    // TODO: this is probably broken with the counter/duration at the end
    /// <summary>
    /// AddNotes(int, int[], int[], int[]) allows programmers to add multiple simultaneous notes, being played on multiple channels. The AddNote() function (see above)
    /// allows the addition of one note, played by itself, for a given duration (1/4 note, 1/2 note, etc.). This function allows programmers
    /// to specify chords of multiple notes.
    /// </summary>
    /// <param name="channel">The track number's channels through which the multiple notes will be played.</param>
    /// <param name="midiNoteNumber">An array of standard MIDI note numbers (see the AddNote() function). The length of this array should match the length of the nDuration[] array.
    /// -1: Rest
    /// </param>
    /// <param name="duration">An array of note lengths (in time ticks) (see the AddNote() function above). The length of this array should match the length of the nMidiNoteNumber[] array. Note durations are defined as below:
    /// 48: Whole Note
    /// 24: Half Note
    /// 12: Quarter Note
    /// 6: Eighth (1/8) Note
    /// 4: Tripolet Note
    /// 3: 1/16 Note
    /// </param>
    let addNotes notes track =
        // It is useful to note here that the time of a MIDI file is cumulative. That is,
        // at any given point, an "offset" or time stamp of 0x00 means "presently" (the time when the last
        // event concluded.
        // Therefore, when adding a note, an initial time-stamp of 0x00 is used.
        // When stopping a note (ending a note), a duration is used as a time-stamp, representing the length of the note.

        // first, we must order the instructions by duration (the ones that end farther in the future must be last).
        // below is code to perform a simple bubble sort.
        let notes = notes |> Seq.sortBy (fun (_, _, duration) -> duration)

        // set a counter to the first note's duration. This will be the quickest
        // duration of the note array, as the note array was previously sorted.
        let counter = notes |> Seq.head |> fun (_, _, duration) -> duration

        let track =
            // go through each of the notes, now sorted. The code below is an exact
            // duplicate of the AddNote() function (above). If the note is a rest, start out with the note not-pressed.
            // Otherwise, we must start with the note pressed.
            notes
            |> Seq.fold
                (fun state (channel, midiNoteNumber, _) ->
                    state
                    |> (if midiNoteNumber = -1 then NoteOff (channel, 0, 0x00uy)
                        else NoteOn (channel, midiNoteNumber, 0x00uy)
                        |> addEvent))
                track
            // stop the first note, as is done with the AddNote() function
            |> (let (channel, midiNoteNumber, _) = Seq.head notes
                NoteOff (channel, (if midiNoteNumber = -1 then 0 else midiNoteNumber), byte counter)
                |> addEvent)

        // go through the rest of the notes. 
        // as each note is stopped, we update counter to reflect the note's duration.
        // This effectively allows us to have "simultaneous" notes of different durations.
        // If a quarter note and a half note are started simultaneously, the quarter note will end
        // before the half note, as the half note has a longer duration.
        notes
        |> Seq.skip 1
        |> Seq.fold
            (fun state (channel, midiNoteNumber, duration) ->
                state
                |> (if midiNoteNumber = -1 then NoteOff (channel, 0, byte counter)
                    else NoteOff (channel, midiNoteNumber, duration - counter |> byte)
                    |> addEvent))
            track

    // TODO: this is probably broken with the counter/duration at the end
    /// <summary>
    /// AddNotes(int, int, int[], int[]) allows programmers to add multiple simultaneous notes. The AddNote() function (see above)
    /// allows the addition of one note, played by itself, for a given duration (1/4 note, 1/2 note, etc.). This function allows programmers
    /// to specify chords of multiple notes.
    /// </summary>
    /// <param name="channel">The track number's channel through which the multiple notes will be played.</param>
    /// <param name="midiNoteNumber">An array of standard MIDI note numbers (see the AddNote() function). The length of this array should match the length of the nDuration[] array.
    /// -1: Rest
    /// </param>
    /// <param name="duration">An array of note lengths (in time ticks) (see the AddNote() function above). The length of this array should match the length of the nMidiNoteNumber[] array. Note durations are defined as below:
    /// 48: Whole Note
    /// 24: Half Note
    /// 12: Quarter Note
    /// 6: Eighth (1/8) Note
    /// 4: Tripolet Note
    /// 3: 1/16 Note
    /// </param>
    let addNotesToChannel channel notes track =
        // It is useful to note here that the time of a MIDI file is cumulative. That is,
        // at any given point, an "offset" or time stamp of 0x00 means "presently" (the time when the last
        // event concluded.
        // Therefore, when adding a note, an initial time-stamp of 0x00 is used.
        // When stopping a note (ending a note), a duration is used as a time-stamp, representing the length of the note.

        // first, we must order the instructions by duration (the ones that end farther in the future must be last).
        // below is code to perform a simple bubble sort.
        let notes = notes |> Seq.sortBy snd
        
        // set a counter to the first note's duration. This will be the quickest
        // duration of the note array, as the note array was previously sorted.
        let counter = notes |> Seq.head |> snd

        let track =
            // go through each of the notes, now sorted. The code below is an exact
            // duplicate of the AddNote() function (above). If the note is a rest, start out with the note not-pressed.
            // Otherwise, we must start with the note pressed.
            notes
            |> Seq.fold
                (fun state (midiNoteNumber, _) ->
                    state
                    |> (if midiNoteNumber = -1 then NoteOff (channel, 0, 0x00uy)
                        else NoteOn (channel, midiNoteNumber, 0x00uy)
                        |> addEvent))
                track
            // stop the first note, as is done with the AddNote() function.
            |> (let midiNoteNumber = Seq.head notes |> fst
                NoteOff (channel, (if midiNoteNumber = -1 then 0 else midiNoteNumber), byte counter)
                |> addEvent)

        // go through the rest of the notes. 
        // as each note is stopped, we update counter to reflect the note's duration.
        // This effectively allows us to have "simultaneous" notes of different durations.
        // If a quarter note and a half note are started simultaneously, the quarter note will end
        // before the half note, as the half note has a longer duration.
        notes
        |> Seq.skip 1
        |> Seq.fold
            (fun state (midiNoteNumber, duration) ->
                state
                |> (if midiNoteNumber = -1 then NoteOff (channel, 0, byte counter)
                    else NoteOff (channel, midiNoteNumber, duration - counter |> byte)
                    |> addEvent))
            track
