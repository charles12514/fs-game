namespace Midi

type ChromaticInstrument =
    | AcousticGrandPiano  | BrightAcousticPiano | ElectricGrandPiano | HonkytonkPiano      | ElectricPiano1      | ElectricPiano2   | Harpsichord      | Clavinet
    | Celesta             | Glockenspiel        | MusicBox           | Vibraphone          | Marimba             | Xylophone        | TubularBells     | Dulcimer
    | DrawbarOrgan        | PercussiveOrgan     | RockOrgan          | ChurchOrgan         | ReedOrgan           | Accordion        | Harmonica        | TangoAccordion
    | AcousticGuitarNylon | AcousticGuitarSteel | ElectricGuitarJazz | ElectricGuitarClean | ElectricGuitarMuted | OverdrivenGuitar | DistortionGuitar | GuitarHarmonics
    | AcousticBass        | ElectricBassFinger  | ElectricBassPick   | FretlessBass        | SlapBass1           | SlapBass2        | SynthBass1       | SynthBass2
    | Violin              | Viola               | Cello              | Contrabass          | TremoloStrings      | PizzicatoStrings | OrchestralHarp   | Timpani
    | StringEnsemble1     | StringEnsemble2     | SynthStrings1      | SynthStrings2       | ChoirAahs           | VoiceOohs        | SynthChoir       | OrchestraHit
    | Trumpet             | Trombone            | Tuba               | MutedTrumpet        | FrenchHorn          | BrassSection     | SynthBrass1      | SynthBrass2
    | SopranoSax          | AltoSax             | TenorSax           | BaritoneSax         | Oboe                | EnglishHorn      | Bassoon          | Clarinet
    | Piccolo             | Flute               | Recorder           | PanFlute            | BlownBottle         | Shakuhachi       | Whistle          | Ocarina
    | Lead1Square         | Lead2Sawtooth       | Lead3Calliope      | Lead4Chiff          | Lead5Charang        | Lead6Voice       | Lead7Fifths      | Lead8BassPlusLead
    | Pad1NewAge          | Pad2Warm            | Pad3Polysynth      | Pad4Choir           | Pad5Bowed           | Pad6Metallic     | Pad7Halo         | Pad8Sweep
    | FX1Rain             | FX2Soundtrack       | FX3Crystal         | FX4Atmosphere       | FX5Brightness       | FX6Goblins       | FX7Echoes        | FX8Scifi
    | Sitar               | Banjo               | Shamisen           | Koto                | Kalimba             | Bagpipe          | Fiddle           | Shanai
    | TinkleBell          | Agogo               | SteelDrums         | Woodblock           | TaikoDrum           | MelodicTom       | SynthDrum        | ReverseCymbal
    | GuitarFretNoise     | BreathNoise         | Seashore           | BirdTweet           | TelephoneRing       | Helicopter       | Applause         | Gunshot
    
    static member op_Explicit chromaticInstrument =
        match chromaticInstrument with
        | AcousticGrandPiano  -> 0   | BrightAcousticPiano -> 1   | ElectricGrandPiano -> 2   | HonkytonkPiano      -> 3   | ElectricPiano1      -> 4   | ElectricPiano2   -> 5   | Harpsichord      -> 6   | Clavinet          -> 7
        | Celesta             -> 8   | Glockenspiel        -> 9   | MusicBox           -> 10  | Vibraphone          -> 11  | Marimba             -> 12  | Xylophone        -> 13  | TubularBells     -> 14  | Dulcimer          -> 15
        | DrawbarOrgan        -> 16  | PercussiveOrgan     -> 17  | RockOrgan          -> 18  | ChurchOrgan         -> 19  | ReedOrgan           -> 20  | Accordion        -> 21  | Harmonica        -> 22  | TangoAccordion    -> 23
        | AcousticGuitarNylon -> 24  | AcousticGuitarSteel -> 25  | ElectricGuitarJazz -> 26  | ElectricGuitarClean -> 27  | ElectricGuitarMuted -> 28  | OverdrivenGuitar -> 29  | DistortionGuitar -> 30  | GuitarHarmonics   -> 31
        | AcousticBass        -> 32  | ElectricBassFinger  -> 33  | ElectricBassPick   -> 34  | FretlessBass        -> 35  | SlapBass1           -> 36  | SlapBass2        -> 37  | SynthBass1       -> 38  | SynthBass2        -> 39
        | Violin              -> 40  | Viola               -> 41  | Cello              -> 42  | Contrabass          -> 43  | TremoloStrings      -> 44  | PizzicatoStrings -> 45  | OrchestralHarp   -> 46  | Timpani           -> 47
        | StringEnsemble1     -> 48  | StringEnsemble2     -> 49  | SynthStrings1      -> 50  | SynthStrings2       -> 51  | ChoirAahs           -> 52  | VoiceOohs        -> 53  | SynthChoir       -> 54  | OrchestraHit      -> 55
        | Trumpet             -> 56  | Trombone            -> 57  | Tuba               -> 58  | MutedTrumpet        -> 59  | FrenchHorn          -> 60  | BrassSection     -> 61  | SynthBrass1      -> 62  | SynthBrass2       -> 63
        | SopranoSax          -> 64  | AltoSax             -> 65  | TenorSax           -> 66  | BaritoneSax         -> 67  | Oboe                -> 68  | EnglishHorn      -> 69  | Bassoon          -> 70  | Clarinet          -> 71
        | Piccolo             -> 72  | Flute               -> 73  | Recorder           -> 74  | PanFlute            -> 75  | BlownBottle         -> 76  | Shakuhachi       -> 77  | Whistle          -> 78  | Ocarina           -> 79
        | Lead1Square         -> 80  | Lead2Sawtooth       -> 81  | Lead3Calliope      -> 82  | Lead4Chiff          -> 83  | Lead5Charang        -> 84  | Lead6Voice       -> 85  | Lead7Fifths      -> 86  | Lead8BassPlusLead -> 87
        | Pad1NewAge          -> 88  | Pad2Warm            -> 89  | Pad3Polysynth      -> 90  | Pad4Choir           -> 91  | Pad5Bowed           -> 92  | Pad6Metallic     -> 93  | Pad7Halo         -> 94  | Pad8Sweep         -> 95
        | FX1Rain             -> 96  | FX2Soundtrack       -> 97  | FX3Crystal         -> 98  | FX4Atmosphere       -> 99  | FX5Brightness       -> 100 | FX6Goblins       -> 101 | FX7Echoes        -> 102 | FX8Scifi          -> 103
        | Sitar               -> 104 | Banjo               -> 105 | Shamisen           -> 106 | Koto                -> 107 | Kalimba             -> 108 | Bagpipe          -> 109 | Fiddle           -> 110 | Shanai            -> 111
        | TinkleBell          -> 112 | Agogo               -> 113 | SteelDrums         -> 114 | Woodblock           -> 115 | TaikoDrum           -> 116 | MelodicTom       -> 117 | SynthDrum        -> 118 | ReverseCymbal     -> 119
        | GuitarFretNoise     -> 120 | BreathNoise         -> 121 | Seashore           -> 122 | BirdTweet           -> 123 | TelephoneRing       -> 124 | Helicopter       -> 125 | Applause         -> 126 | Gunshot           -> 127
        
    static member ofInt =
        function
        | 0   -> AcousticGrandPiano  | 1   -> BrightAcousticPiano | 2   -> ElectricGrandPiano | 3   -> HonkytonkPiano      | 4   -> ElectricPiano1      | 5   -> ElectricPiano2   | 6   -> Harpsichord      | 7   -> Clavinet
        | 8   -> Celesta             | 9   -> Glockenspiel        | 10  -> MusicBox           | 11  -> Vibraphone          | 12  -> Marimba             | 13  -> Xylophone        | 14  -> TubularBells     | 15  -> Dulcimer
        | 16  -> DrawbarOrgan        | 17  -> PercussiveOrgan     | 18  -> RockOrgan          | 19  -> ChurchOrgan         | 20  -> ReedOrgan           | 21  -> Accordion        | 22  -> Harmonica        | 23  -> TangoAccordion
        | 24  -> AcousticGuitarNylon | 25  -> AcousticGuitarSteel | 26  -> ElectricGuitarJazz | 27  -> ElectricGuitarClean | 28  -> ElectricGuitarMuted | 29  -> OverdrivenGuitar | 30  -> DistortionGuitar | 31  -> GuitarHarmonics
        | 32  -> AcousticBass        | 33  -> ElectricBassFinger  | 34  -> ElectricBassPick   | 35  -> FretlessBass        | 36  -> SlapBass1           | 37  -> SlapBass2        | 38  -> SynthBass1       | 39  -> SynthBass2
        | 40  -> Violin              | 41  -> Viola               | 42  -> Cello              | 43  -> Contrabass          | 44  -> TremoloStrings      | 45  -> PizzicatoStrings | 46  -> OrchestralHarp   | 47  -> Timpani
        | 48  -> StringEnsemble1     | 49  -> StringEnsemble2     | 50  -> SynthStrings1      | 51  -> SynthStrings2       | 52  -> ChoirAahs           | 53  -> VoiceOohs        | 54  -> SynthChoir       | 55  -> OrchestraHit
        | 56  -> Trumpet             | 57  -> Trombone            | 58  -> Tuba               | 59  -> MutedTrumpet        | 60  -> FrenchHorn          | 61  -> BrassSection     | 62  -> SynthBrass1      | 63  -> SynthBrass2
        | 64  -> SopranoSax          | 65  -> AltoSax             | 66  -> TenorSax           | 67  -> BaritoneSax         | 68  -> Oboe                | 69  -> EnglishHorn      | 70  -> Bassoon          | 71  -> Clarinet
        | 72  -> Piccolo             | 73  -> Flute               | 74  -> Recorder           | 75  -> PanFlute            | 76  -> BlownBottle         | 77  -> Shakuhachi       | 78  -> Whistle          | 79  -> Ocarina
        | 80  -> Lead1Square         | 81  -> Lead2Sawtooth       | 82  -> Lead3Calliope      | 83  -> Lead4Chiff          | 84  -> Lead5Charang        | 85  -> Lead6Voice       | 86  -> Lead7Fifths      | 87  -> Lead8BassPlusLead
        | 88  -> Pad1NewAge          | 89  -> Pad2Warm            | 90  -> Pad3Polysynth      | 91  -> Pad4Choir           | 92  -> Pad5Bowed           | 93  -> Pad6Metallic     | 94  -> Pad7Halo         | 95  -> Pad8Sweep
        | 96  -> FX1Rain             | 97  -> FX2Soundtrack       | 98  -> FX3Crystal         | 99  -> FX4Atmosphere       | 100 -> FX5Brightness       | 101 -> FX6Goblins       | 102 -> FX7Echoes        | 103 -> FX8Scifi
        | 104 -> Sitar               | 105 -> Banjo               | 106 -> Shamisen           | 107 -> Koto                | 108 -> Kalimba             | 109 -> Bagpipe          | 110 -> Fiddle           | 111 -> Shanai
        | 112 -> TinkleBell          | 113 -> Agogo               | 114 -> SteelDrums         | 115 -> Woodblock           | 116 -> TaikoDrum           | 117 -> MelodicTom       | 118 -> SynthDrum        | 119 -> ReverseCymbal
        | 120 -> GuitarFretNoise     | 121 -> BreathNoise         | 122 -> Seashore           | 123 -> BirdTweet           | 124 -> TelephoneRing       | 125 -> Helicopter       | 126 -> Applause         | 127 -> Gunshot
        | x -> sprintf "Invalid instrument number %d" x |> failwith
        
type ChromaticInstrumentClass =
    | Piano | ChromaticPercussion | Organ | Guitar | Bass | Strings | Ensemble | Brass | Reed | Pipe | SynthLead | SynthPad | SynthEffects | Ethnic | Percussive | SoundEffects
    
    static member ofInstrument =
        function
        | AcousticGrandPiano  | BrightAcousticPiano | ElectricGrandPiano | HonkytonkPiano      | ElectricPiano1      | ElectricPiano2   | Harpsichord      | Clavinet          -> Piano
        | Celesta             | Glockenspiel        | MusicBox           | Vibraphone          | Marimba             | Xylophone        | TubularBells     | Dulcimer          -> ChromaticPercussion
        | DrawbarOrgan        | PercussiveOrgan     | RockOrgan          | ChurchOrgan         | ReedOrgan           | Accordion        | Harmonica        | TangoAccordion    -> Organ
        | AcousticGuitarNylon | AcousticGuitarSteel | ElectricGuitarJazz | ElectricGuitarClean | ElectricGuitarMuted | OverdrivenGuitar | DistortionGuitar | GuitarHarmonics   -> Guitar
        | AcousticBass        | ElectricBassFinger  | ElectricBassPick   | FretlessBass        | SlapBass1           | SlapBass2        | SynthBass1       | SynthBass2        -> Bass
        | Violin              | Viola               | Cello              | Contrabass          | TremoloStrings      | PizzicatoStrings | OrchestralHarp   | Timpani           -> Strings
        | StringEnsemble1     | StringEnsemble2     | SynthStrings1      | SynthStrings2       | ChoirAahs           | VoiceOohs        | SynthChoir       | OrchestraHit      -> Ensemble
        | Trumpet             | Trombone            | Tuba               | MutedTrumpet        | FrenchHorn          | BrassSection     | SynthBrass1      | SynthBrass2       -> Brass
        | SopranoSax          | AltoSax             | TenorSax           | BaritoneSax         | Oboe                | EnglishHorn      | Bassoon          | Clarinet          -> Reed
        | Piccolo             | Flute               | Recorder           | PanFlute            | BlownBottle         | Shakuhachi       | Whistle          | Ocarina           -> Pipe
        | Lead1Square         | Lead2Sawtooth       | Lead3Calliope      | Lead4Chiff          | Lead5Charang        | Lead6Voice       | Lead7Fifths      | Lead8BassPlusLead -> SynthLead
        | Pad1NewAge          | Pad2Warm            | Pad3Polysynth      | Pad4Choir           | Pad5Bowed           | Pad6Metallic     | Pad7Halo         | Pad8Sweep         -> SynthPad
        | FX1Rain             | FX2Soundtrack       | FX3Crystal         | FX4Atmosphere       | FX5Brightness       | FX6Goblins       | FX7Echoes        | FX8Scifi          -> SynthEffects
        | Sitar               | Banjo               | Shamisen           | Koto                | Kalimba             | Bagpipe          | Fiddle           | Shanai            -> Ethnic
        | TinkleBell          | Agogo               | SteelDrums         | Woodblock           | TaikoDrum           | MelodicTom       | SynthDrum        | ReverseCymbal     -> Percussive
        | GuitarFretNoise     | BreathNoise         | Seashore           | BirdTweet           | TelephoneRing       | Helicopter       | Applause         | Gunshot           -> SoundEffects
        
    static member toInstrumentList =
        function
        | Piano               -> [ AcousticGrandPiano ; BrightAcousticPiano; ElectricGrandPiano; HonkytonkPiano     ; ElectricPiano1     ; ElectricPiano2  ; Harpsichord     ; Clavinet          ]
        | ChromaticPercussion -> [ Celesta            ; Glockenspiel       ; MusicBox          ; Vibraphone         ; Marimba            ; Xylophone       ; TubularBells    ; Dulcimer          ]
        | Organ               -> [ DrawbarOrgan       ; PercussiveOrgan    ; RockOrgan         ; ChurchOrgan        ; ReedOrgan          ; Accordion       ; Harmonica       ; TangoAccordion    ]
        | Guitar              -> [ AcousticGuitarNylon; AcousticGuitarSteel; ElectricGuitarJazz; ElectricGuitarClean; ElectricGuitarMuted; OverdrivenGuitar; DistortionGuitar; GuitarHarmonics   ]
        | Bass                -> [ AcousticBass       ; ElectricBassFinger ; ElectricBassPick  ; FretlessBass       ; SlapBass1          ; SlapBass2       ; SynthBass1      ; SynthBass2        ]
        | Strings             -> [ Violin             ; Viola              ; Cello             ; Contrabass         ; TremoloStrings     ; PizzicatoStrings; OrchestralHarp  ; Timpani           ]
        | Ensemble            -> [ StringEnsemble1    ; StringEnsemble2    ; SynthStrings1     ; SynthStrings2      ; ChoirAahs          ; VoiceOohs       ; SynthChoir      ; OrchestraHit      ]
        | Brass               -> [ Trumpet            ; Trombone           ; Tuba              ; MutedTrumpet       ; FrenchHorn         ; BrassSection    ; SynthBrass1     ; SynthBrass2       ]
        | Reed                -> [ SopranoSax         ; AltoSax            ; TenorSax          ; BaritoneSax        ; Oboe               ; EnglishHorn     ; Bassoon         ; Clarinet          ]
        | Pipe                -> [ Piccolo            ; Flute              ; Recorder          ; PanFlute           ; BlownBottle        ; Shakuhachi      ; Whistle         ; Ocarina           ]
        | SynthLead           -> [ Lead1Square        ; Lead2Sawtooth      ; Lead3Calliope     ; Lead4Chiff         ; Lead5Charang       ; Lead6Voice      ; Lead7Fifths     ; Lead8BassPlusLead ]
        | SynthPad            -> [ Pad1NewAge         ; Pad2Warm           ; Pad3Polysynth     ; Pad4Choir          ; Pad5Bowed          ; Pad6Metallic    ; Pad7Halo        ; Pad8Sweep         ]
        | SynthEffects        -> [ FX1Rain            ; FX2Soundtrack      ; FX3Crystal        ; FX4Atmosphere      ; FX5Brightness      ; FX6Goblins      ; FX7Echoes       ; FX8Scifi          ]
        | Ethnic              -> [ Sitar              ; Banjo              ; Shamisen          ; Koto               ; Kalimba            ; Bagpipe         ; Fiddle          ; Shanai            ]
        | Percussive          -> [ TinkleBell         ; Agogo              ; SteelDrums        ; Woodblock          ; TaikoDrum          ; MelodicTom      ; SynthDrum       ; ReverseCymbal     ]
        | SoundEffects        -> [ GuitarFretNoise    ; BreathNoise        ; Seashore          ; BirdTweet          ; TelephoneRing      ; Helicopter      ; Applause        ; Gunshot           ]
        
type PercussionInstrument =
    | BassDrum2   | BassDrum1     | Rimshot      | SnareDrum1    | HandClap      | SnareDrum2   | LowTom2      | ClosedHihat
    | LowTom1     | PedalHihat    | MidTom2      | OpenHihat     | MidTom1       | HighTom2     | CrashCymbal1 | HighTom1
    | RideCymbal1 | ChineseCymbal | RideBell     | Tambourine    | SplashCymbal  | Cowbell      | CrashCymbal2 | VibraSlap
    | RideCymbal2 | HighBongo     | LowBongo     | MuteHighConga | OpenHighConga | LowConga     | HighTimbale  | LowTimbale
    | HighAgogo   | LowAgogo      | Cabasa       | Maracas       | ShortWhistle  | LongWhistle  | ShortGuiro   | LongGuiro
    | Claves      | HighWoodBlock | LowWoodBlock | MuteCuica     | OpenCuica     | MuteTriangle | OpenTriangle
    
    static member op_Explicit percussionInstrument =
        match percussionInstrument with
        | BassDrum2   -> 35 | BassDrum1     -> 36 | Rimshot      -> 37 | SnareDrum1    -> 38 | HandClap      -> 39 | SnareDrum2   -> 40 | LowTom2      -> 41 | ClosedHihat -> 42
        | LowTom1     -> 43 | PedalHihat    -> 44 | MidTom2      -> 45 | OpenHihat     -> 46 | MidTom1       -> 47 | HighTom2     -> 48 | CrashCymbal1 -> 49 | HighTom1    -> 50
        | RideCymbal1 -> 51 | ChineseCymbal -> 52 | RideBell     -> 53 | Tambourine    -> 54 | SplashCymbal  -> 55 | Cowbell      -> 56 | CrashCymbal2 -> 57 | VibraSlap   -> 58
        | RideCymbal2 -> 59 | HighBongo     -> 60 | LowBongo     -> 61 | MuteHighConga -> 62 | OpenHighConga -> 63 | LowConga     -> 64 | HighTimbale  -> 65 | LowTimbale  -> 66
        | HighAgogo   -> 67 | LowAgogo      -> 68 | Cabasa       -> 69 | Maracas       -> 70 | ShortWhistle  -> 71 | LongWhistle  -> 72 | ShortGuiro   -> 73 | LongGuiro   -> 74
        | Claves      -> 75 | HighWoodBlock -> 76 | LowWoodBlock -> 77 | MuteCuica     -> 78 | OpenCuica     -> 79 | MuteTriangle -> 80 | OpenTriangle -> 81
        
    static member ofInt =
        function
        | 35 -> BassDrum2   | 36 -> BassDrum1     | 37 -> Rimshot      | 38 -> SnareDrum1    | 39 -> HandClap      | 40 -> SnareDrum2   | 41 -> LowTom2      | 42 -> ClosedHihat
        | 43 -> LowTom1     | 44 -> PedalHihat    | 45 -> MidTom2      | 46 -> OpenHihat     | 47 -> MidTom1       | 48 -> HighTom2     | 49 -> CrashCymbal1 | 50 -> HighTom1
        | 51 -> RideCymbal1 | 52 -> ChineseCymbal | 53 -> RideBell     | 54 -> Tambourine    | 55 -> SplashCymbal  | 56 -> Cowbell      | 57 -> CrashCymbal2 | 58 -> VibraSlap
        | 59 -> RideCymbal2 | 60 -> HighBongo     | 61 -> LowBongo     | 62 -> MuteHighConga | 63 -> OpenHighConga | 64 -> LowConga     | 65 -> HighTimbale  | 66 -> LowTimbale
        | 67 -> HighAgogo   | 68 -> LowAgogo      | 69 -> Cabasa       | 70 -> Maracas       | 71 -> ShortWhistle  | 72 -> LongWhistle  | 73 -> ShortGuiro   | 74 -> LongGuiro
        | 75 -> Claves      | 76 -> HighWoodBlock | 77 -> LowWoodBlock | 78 -> MuteCuica     | 79 -> OpenCuica     | 80 -> MuteTriangle | 81 -> OpenTriangle
        | x -> sprintf "Invalid percussion pitch %d" x |> failwith
        
type PercussionInstrumentClass =
    | Bass | Bell | Blocks | CrashCymbal | HighTom | LowTom | Rattle | RideCymbal | Snare
    
    static member ofInstrument =
        function
        | BassDrum1     | BassDrum2                                                                                             -> Bass
        | Cowbell       | HighAgogo    | LongWhistle   | LowAgogo    | MuteTriangle | OpenTriangle  | RideBell  | ShortWhistle  -> Bell
        | Claves        | HandClap     | HighWoodBlock | LowWoodBlock                                                           -> Blocks
        | ChineseCymbal | CrashCymbal1 | CrashCymbal2                                                                           -> CrashCymbal
        | HighBongo     | HighTimbale  | HighTom1      | HighTom2    | MuteCuica    | MuteHighConga | OpenCuica | OpenHighConga -> HighTom
        | LowBongo      | LowConga     | LowTimbale    | LowTom1     | LowTom2      | MidTom1       | MidTom2                   -> LowTom
        | Cabasa        | LongGuiro    | Maracas       | ShortGuiro  | Tambourine   | VibraSlap                                 -> Rattle
        | ClosedHihat   | OpenHihat    | PedalHihat    | RideCymbal1 | RideCymbal2  | SplashCymbal                              -> RideCymbal
        | Rimshot       | SnareDrum1   | SnareDrum2                                                                             -> Snare
        
    static member toInstrumentList =
        function
        | Bass ->        [ BassDrum1    ; BassDrum2                                                                                        ]
        | Bell ->        [ Cowbell      ; HighAgogo   ; LongWhistle  ; LowAgogo    ; MuteTriangle; OpenTriangle ; RideBell ; ShortWhistle  ]
        | Blocks ->      [ Claves       ; HandClap    ; HighWoodBlock; LowWoodBlock                                                        ]
        | CrashCymbal -> [ ChineseCymbal; CrashCymbal1; CrashCymbal2                                                                       ]
        | HighTom ->     [ HighBongo    ; HighTimbale ; HighTom1     ; HighTom2    ; MuteCuica   ; MuteHighConga; OpenCuica; OpenHighConga ]
        | LowTom ->      [ LowBongo     ; LowConga    ; LowTimbale   ; LowTom1     ; LowTom2     ; MidTom1      ; MidTom2                  ]
        | Rattle ->      [ Cabasa       ; LongGuiro   ; Maracas      ; ShortGuiro  ; Tambourine  ; VibraSlap                               ]
        | RideCymbal ->  [ ClosedHihat  ; OpenHihat   ; PedalHihat   ; RideCymbal1 ; RideCymbal2 ; SplashCymbal                            ]
        | Snare ->       [ Rimshot      ; SnareDrum1  ; SnareDrum2                                                                         ]
        
    static member op_Explicit percussionInstrumentClass =
        match percussionInstrumentClass with
        | Bass -> 0 | Bell -> 1 | Blocks -> 2 | CrashCymbal -> 3 | HighTom -> 4 | LowTom -> 5 | Rattle -> 6 | RideCymbal -> 7 | Snare -> 8
        
    static member ofInt =
        function
        | 0 -> Bass | 1 -> Bell | 2 -> Blocks | 3 -> CrashCymbal | 4 -> HighTom | 5 -> LowTom | 6 -> Rattle | 7 -> RideCymbal | 8 -> Snare
        | x -> sprintf "Invalid percussion class %d" x |> failwith

type Duration =
    | Whole = 48
    | Half = 24
    | Quarter = 12
    | Eighth = 6
    | Tripolet = 4
    | Sixteenth = 3
