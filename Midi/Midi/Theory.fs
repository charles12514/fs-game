namespace Midi

module Scale =
    let create (offsets:int array) (root:Note) =
        offsets
        |> Array.map
            (fun offset -> offset + int root |> Note.ofInt)
    let inOctaves low high scale =
        let low = max low Octave.MinValue
        let high = min high Octave.MaxValue
        [| low .. high |]
        |> Array.collect
            (fun octave ->
                let octave = Octave.make octave
                scale
                |> Array.choose
                    (fun note ->
                        { Note = note; Octave = octave }
                        |> NoteOctave.tryPitch))
    let major = create [| 0; 2; 4; 5; 7; 9; 11 |]
    let minor = create [| 0; 2; 3; 5; 7; 8; 10 |]

    // Thanks to http://biteyourownelbow.com/keychar.htm for these lovely descriptions
    let ``C major``  = major C      // Completely pure. Its character is: innocence, simplicity, naïvety, children's talk.
    let ``C# major`` = major ``C#``
    let ``Db major`` = major Db     // A leering key, degenerating into grief and rapture. It cannot laugh, but it can smile; it cannot howl, but it can at least grimace its crying.--Consequently only unusual characters and feelings can be brought out in this key.
    let ``D major``  = major D      // The key of triumph, of Hallejuahs, of war-cries, of victory-rejoicing. Thus, the inviting symphonies, the marches, holiday songs and heaven-rejoicing choruses are set in this key.
    let ``D# major`` = major ``D#``
    let ``Eb major`` = major Eb     // The key of love, of devotion, of intimate conversation with God.
    let ``E major``  = major E      // Noisy shouts of joy, laughing pleasure and not yet complete, full delight lies in E Major.
    let ``F major``  = major F      // Complaisance & calm.
    let ``F# major`` = major ``F#`` // Triumph over difficulty, free sigh of relief utered when hurdles are surmounted; echo of a soul which has fiercely struggled and finally conquered lies in all uses of this key.
    let ``Gb major`` = major Gb
    let ``G major``  = major G      // Everything rustic, idyllic and lyrical, every calm and satisfied passion, every tender gratitude for true friendship and faithful love,--in a word every gentle and peaceful emotion of the heart is correctly expressed by this key.
    let ``G# major`` = major ``G#``
    let ``Ab major`` = major Ab     // Key of the grave. Death, grave, putrefaction, judgment, eternity lie in its radius.
    let ``A major``  = major A      // This key includes declarations of innocent love, satisfaction with one's state of affairs; hope of seeing one's beloved again when parting; youthful cheerfulness and trust in God.
    let ``A# major`` = major ``A#``
    let ``Bb major`` = major Bb     // Cheerful love, clear conscience, hope aspiration for a better world.
    let ``B major``  = major B      // Strongly coloured, announcing wild passions, composed from the most glaring coulors. Anger, rage, jealousy, fury, despair and every burden of the heart lies in its sphere.

    let ``C minor``  = minor C      // Declaration of love and at the same time the lament of unhappy love. All languishing, longing, sighing of the love-sick soul lies in this key.
    let ``C# minor`` = minor ``C#`` // Penitential lamentation, intimate conversation with God
    let ``Db minor`` = minor Db
    let ``D minor``  = minor D      // Melancholy womanliness, the spleen and humours brood.
    let ``D# minor`` = minor ``D#`` // Feelings of the anxiety of the soul's deepest distress, of brooding despair, of blackest depresssion, of the most gloomy condition of the soul. Every fear, every hesitation of the shuddering heart, breathes out of horrible D# minor. If ghosts could speak, their speech would approximate this key.
    let ``Eb minor`` = minor Eb
    let ``E minor``  = minor E      // effeminate, amorous, plaintive
    let ``F minor``  = minor F      // Deep depression, funereal lament, groans of misery and longing for the grave.
    let ``F# minor`` = minor ``F#`` // A gloomy key: it tugs at passion as a dog biting a dress. Resentment and discontent are its language.
    let ``Gb minor`` = minor Gb
    let ``G minor``  = minor G      // Discontent, uneasiness, worry about a failed scheme; bad-tempered gnashing of teeth; in a word: resentment and dislike.
    let ``G# minor`` = minor ``G#``
    let ``Ab minor`` = minor Ab     // Grumbler, heart squeezed until it suffocates; wailing lament, difficult struggle; in a word, the color of this key is everything struggling with difficulty.
    let ``A minor``  = minor A      // Pious womanliness and tenderness of character.
    let ``A# minor`` = minor ``A#``
    let ``Bb minor`` = minor Bb     // A quaint creature, often dressed in the garment of night. It is somewhat surly and very seldom takes on a pleasant countenance. Mocking God and the world; discontented with itself and with everything; preparation for suicide sounds in this key.
    let ``B minor``  = minor B      // This is as it were the key of patience, of calm awaiting ones's fate and of submission to divine dispensation.

module Chord =
    let major root =
        let scale = Scale.major root
        [| scale.[0]; scale.[2]; scale.[4] |]
    let minor root =
        let scale = Scale.major root
        [| scale.[0]; int scale.[2] - 1 |> Note.ofInt; scale.[4] |]
    let sixth root =
        let scale = Scale.major root
        [| scale.[0]; scale.[2]; scale.[4]; scale.[5] |]
    let seventh root =
        let scale = Scale.major root
        [| scale.[0]; scale.[2]; scale.[4]; scale.[6] |]

module Neck =
    module Tuning =
        let standard =
            [ { Note = E; Octave = Octave.make 3 }
              { Note = A; Octave = Octave.make 3 }
              { Note = D; Octave = Octave.make 4 }
              { Note = G; Octave = Octave.make 4 }
              { Note = B; Octave = Octave.make 4 }
              { Note = E; Octave = Octave.make 5 } ]
    let find tuning scale =
        tuning
        |> List.map
            (fun openString ->
                [ 0 .. Octave.length - 1 ]
                |> List.choose
                    (fun interval ->
                        let note = openString.Note + interval
                        if scale |> Seq.contains note then
                            Some interval
                        else None))
