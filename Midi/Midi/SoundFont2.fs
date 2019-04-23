module Midi.SoundFont2

open System
open System.Collections.Generic

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Parser =
    module Riff =
        type Chunk =
          { Type : string
            Size : int
            Offset : int }
        [<RequireQualifiedAccess>]
        module Parser =
            let parseChunk (input:byte array) ip padding offset bigEndian =
                let chunkType =
                    input.[ip .. ip + 3]
                    |> Array.map char
                    |> fun s -> String.Join ("", s)
                let size =
                    input.[ip + 4 .. ip + 7]
                    |> if bigEndian then id
                       else Array.rev
                    |> Seq.fold
                        (fun n b -> (n <<< 8) ||| int b)
                        0
                let chunk =
                  { Type = chunkType
                    Size = size
                    Offset = ip + 8 }
                let ip = ip + 8 + size
                let ip =
                    if padding && ((ip - offset) &&& 1) = 1 then ip + 1
                    else ip
                (chunk, ip)
            let parse (input:byte array) ip length padding bigEndian =
                let ip =
                    match ip with
                    | Some ip -> ip
                    | None -> 0
                let length =
                    match length with
                    | Some length -> length
                    | None -> input.Length
                let offset = ip
                let padding =
                    match padding with
                    | Some padding -> padding
                    | None -> true
                let bigEndian =
                    match bigEndian with
                    | Some bigEndian -> bigEndian
                    | None -> false
                let length = length + offset
                let mutable ip = ip
                seq {
                    while ip < length do
                        let (chunk, i) = parseChunk input ip padding offset bigEndian
                        yield chunk
                        ip <- i
                }
    let GeneratorEnumeratorTable = // TODO DU
     [| "startAddrsOffset"
        "endAddrsOffset"
        "startloopAddrsOffset"
        "endloopAddrsOffset"
        "startAddrsCoarseOffset"
        "modLfoToPitch"
        "vibLfoToPitch"
        "modEnvToPitch"
        "initialFilterFc"
        "initialFilterQ"
        "modLfoToFilterFc"
        "modEnvToFilterFc"
        "endAddrsCoarseOffset"
        "modLfoToVolume"
        null // undefined, 14
        "chorusEffectsSend"
        "reverbEffectsSend"
        "pan"
        null // undefined, 18
        null // undefined, 19
        null // undefined, 20
        "delayModLFO"
        "freqModLFO"
        "delayVibLFO"
        "freqVibLFO"
        "delayModEnv"
        "attackModEnv"
        "holdModEnv"
        "decayModEnv"
        "sustainModEnv"
        "releaseModEnv"
        "keynumToModEnvHold"
        "keynumToModEnvDecay"
        "delayVolEnv"
        "attackVolEnv"
        "holdVolEnv"
        "decayVolEnv"
        "sustainVolEnv"
        "releaseVolEnv"
        "keynumToVolEnvHold"
        "keynumToVolEnvDecay"
        "instrument"
        null // undefined, 42
        "keyRange"
        "velRange"
        "startloopAddrsCoarseOffset"
        "keynum"
        "velocity"
        "initialAttenuation"
        null // undefined, 49
        "endloopAddrsCoarseOffset"
        "coarseTune"
        "fineTune"
        "sampleID"
        "sampleModes"
        null // undefined, 55
        "scaleTuning"
        "exclusiveClass"
        "overridingRootKey" |]
    type PresetHeader =
      { PresetName : string
        Preset : uint16
        Bank : uint16
        PresetBagIndex : int
        Library : int
        Genre : int
        Morphology : int }
    type PresetZone =
      { PresetGeneratorIndex : int
        PresetModulatorIndex : int }
    type Instrument =
      { InstrumentName : string
        InstrumentBagIndex : int }
    type InstrumentZone =
      { InstrumentGeneratorIndex : int
        InstrumentModulatorIndex : int }
    type SampleHeader =
      { SampleName : string
//        Start : int
//        End : int
        StartLoop : int
        EndLoop : int
        SampleRate : int
        OriginalPitch : byte
        PitchCorrection : int
        SampleLink : uint16
        SampleType : uint16 }
    type ModulatorOrGenerator =
      { Type : string
        Code : uint16
        Amount : int
        Lo : byte
        Hi : byte }
    type Preset =
      { PresetHeaders : PresetHeader array
        PresetZones : PresetZone array
        PresetZoneModulator : ModulatorOrGenerator array
        PresetZoneGenerator : ModulatorOrGenerator array
        Instruments : Instrument array
        InstrumentZones : InstrumentZone array
        InstrumentZoneModulator : ModulatorOrGenerator array
        InstrumentZoneGenerator : ModulatorOrGenerator array
        SampleHeaders : (uint16 array * SampleHeader) array }
    type Soundbank =
      { Info : Riff.Chunk array
        SamplingData : Riff.Chunk
        PresetData : Preset }
    let parseShort (input:byte array) (start:int) =
        uint16 input.[start] ||| (uint16 input.[start + 1] <<< 8)
    let parseInt (input:byte array) (start:int) =
        int input.[start] ||| (int input.[start + 1] <<< 8) ||| (int input.[start + 2] <<< 16) ||| (int input.[start + 3] <<< 24)
    let parseString (input:byte array) start length =
        input.[start .. start + length - 1]
        |> Array.takeWhile ((<>) 0uy)
        |> Array.map char
        |> String
    let parseModulator (input:byte array) (chunk:Riff.Chunk) =
        let mutable ip = chunk.Offset
        let size = chunk.Offset + chunk.Size
        let mutable output = []
        while ip < size do
            ip <- ip + 2
            let code = parseShort input ip
            ip <- ip + 2
            let key = GeneratorEnumeratorTable.[int code]
            output <-
                { Type = key
                  Code = code
                  Amount = ((parseShort input ip |> int) <<< 16) >>> 16
                  Lo = input.[ip]
                  Hi = input.[ip + 1] }
                :: output
            ip <- ip + 2
            ip <- ip + 2
            ip <- ip + 2
        List.rev output
        |> List.toArray
    let parseGenerator (input:byte array) (chunk:Riff.Chunk) =
        let mutable ip = chunk.Offset
        let size = chunk.Offset + chunk.Size
        let mutable output = []
        while ip < size do
            let code = parseShort input ip
            ip <- ip + 2
            let key = GeneratorEnumeratorTable.[int code]
            output <-
                { Type = key
                  Code = code
                  Amount = ((parseShort input ip |> int) <<< 16) >>> 16
                  Lo = input.[ip]
                  Hi = input.[ip + 1] }
                :: output
            ip <- ip + 2
        List.rev output
        |> List.toArray
    let parseInfoList (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "LIST" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let ip = chunk.Offset
            let signature =
                input.[ip .. ip + 3]
                |> Array.map char
                |> fun s -> String.Join ("", s)
            if signature <> "INFO" then
                failwithf "invalid signature: %s" signature
            else
                Riff.Parser.parse input (ip + 4 |> Some) (chunk.Size - 4 |> Some) None None
                |> Seq.toArray
    let parseSdtaList (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "LIST" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let ip = chunk.Offset
            let signature =
                input.[ip .. ip + 3]
                |> Array.map char
                |> fun s -> String.Join ("", s)
            if signature <> "sdta" then
                failwithf "invalid signature: %s" signature
            else
                Riff.Parser.parse input (ip + 4 |> Some) (chunk.Size - 4 |> Some) None None
                |> Seq.exactlyOne
    let parsePhdr (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "phdr" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let mutable ip = chunk.Offset
            let size = chunk.Offset + chunk.Size
            let mutable presetHeaders = []
            while ip < size do
                presetHeaders <-
                    {   PresetName = parseString input ip 20
                        Preset = parseShort input (ip + 20)
                        Bank = parseShort input (ip + 22)
                        PresetBagIndex = parseShort input (ip + 24) |> int
                        Library = parseInt input (ip + 26)
                        Genre = parseInt input (ip + 30)
                        Morphology = parseInt input (ip + 34) }
                    :: presetHeaders
                ip <- ip + 38
            List.rev presetHeaders
            |> List.toArray
    let parsePbag (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "pbag" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let mutable ip = chunk.Offset
            let size = chunk.Offset + chunk.Size
            let mutable presetZones = []
            while ip < size do
                presetZones <-
                    { PresetGeneratorIndex = parseShort input ip |> int
                      PresetModulatorIndex = parseShort input (ip + 2) |> int }
                    :: presetZones
                ip <- ip + 4
            List.rev presetZones
            |> List.toArray
    let parsePmod (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "pmod" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            parseModulator input chunk
    let parsePgen (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "pgen" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            parseGenerator input chunk
    let parseInst (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "inst" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let mutable ip = chunk.Offset
            let size = chunk.Offset + chunk.Size
            let mutable instruments = []
            while ip < size do
                instruments <-
                    { InstrumentName = parseString input ip 20
                      InstrumentBagIndex = parseShort input (ip + 20) |> int }
                    :: instruments
                ip <- ip + 22
            List.rev instruments
            |> List.toArray
    let parseIbag (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "ibag" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let mutable ip = chunk.Offset
            let size = chunk.Offset + chunk.Size
            let mutable instrumentZone = []
            while ip < size do
                instrumentZone <-
                    { InstrumentGeneratorIndex = parseShort input ip |> int
                      InstrumentModulatorIndex = parseShort input (ip + 2) |> int }
                    :: instrumentZone
                ip <- ip + 4
            List.rev instrumentZone
            |> List.toArray
    let parseImod (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "imod" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            parseModulator input chunk
    let parseIgen (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "igen" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            parseGenerator input chunk
    // TODO: This function is questionable;
    // it doesn't interpolate the sample data
    // and always forces a sample rate of 22050 or higher. Why?
    let adjustSampleData sample sampleRate =
        let mutable multiply = 1
        let mutable sample = sample
        let mutable sampleRate = sampleRate
        while sampleRate < 22050 do
            sample <-
                sample
                |> Array.collect (fun x -> [| x; x |])
            multiply <- multiply * 2
            sampleRate <- sampleRate * 2
        (sample, multiply)
    let parseShdr (input:byte array) (samplingData:Riff.Chunk) (chunk:Riff.Chunk) =
        if chunk.Type <> "shdr" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let mutable ip = chunk.Offset
            let mutable samples = [] // (sample, sampleheader) list
            let size = chunk.Offset + chunk.Size

            while ip < size do
                let sampleName = parseString input ip 20
                let start = parseInt input (ip + 20)
                let ``end`` = parseInt input (ip + 24)
                let startLoop = parseInt input (ip + 28)
                let endLoop = parseInt input (ip + 32)
                let sampleRate = parseInt input (ip + 36)
                let originalPitch = input.[ip + 40]
                let pitchCorrection = (int input.[ip + 41] <<< 24) >>> 24
                let sampleLink = parseShort input (ip + 42)
                let sampleType = parseShort input (ip + 44)
                let sample =
                    let raw = input.[samplingData.Offset + start * 2 .. samplingData.Offset + ``end`` * 2 - 1]
                    [| 0 .. ``end`` - start - 1 |]
                    |> Array.map (fun i -> parseShort raw (i * 2))
                let startLoop = startLoop - start
                let endLoop = endLoop - start
                let (sample, sampleRate, startLoop, endLoop) =
                    if sampleRate > 0 then
                        let (sample, multiply) = adjustSampleData sample sampleRate
                        (sample, sampleRate * multiply, startLoop * multiply, endLoop * multiply)
                    else
                        (sample, sampleRate, startLoop, endLoop)
                let sampleHeader =
                  { SampleName = sampleName
                    StartLoop = startLoop
                    EndLoop = endLoop
                    SampleRate = sampleRate
                    OriginalPitch = originalPitch
                    PitchCorrection = pitchCorrection
                    SampleLink = sampleLink
                    SampleType = sampleType }
                samples <- (sample, sampleHeader) :: samples
                ip <- ip + 46
                ()
            List.rev samples
            |> List.toArray
    let parsePdtaList (input:byte array) (samplingData:Riff.Chunk) (chunk:Riff.Chunk) =
        if chunk.Type <> "LIST" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let ip = chunk.Offset
            let signature =
                input.[ip .. ip + 3]
                |> Array.map char
                |> fun s -> String.Join ("", s)
            if signature <> "pdta" then
                failwithf "invalid signature: %s" signature
            else
                Riff.Parser.parse input (ip + 4 |> Some) (chunk.Size - 4 |> Some) None None
                |> Seq.toArray
                |> function
                    | [| c0; c1; c2; c3; c4; c5; c6; c7; c8 |] ->
                        {   PresetHeaders = parsePhdr input c0
                            PresetZones = parsePbag input c1
                            PresetZoneModulator = parsePmod input c2
                            PresetZoneGenerator = parsePgen input c3
                            Instruments = parseInst input c4
                            InstrumentZones = parseIbag input c5
                            InstrumentZoneModulator = parseImod input c6
                            InstrumentZoneGenerator = parseIgen input c7
                            SampleHeaders = parseShdr input samplingData c8 }
                    | _ -> failwith "invalid pdta chunk"
    let parseRiffChunk (input:byte array) (chunk:Riff.Chunk) =
        if chunk.Type <> "RIFF" then
            failwithf "invalid chunk type: %s" chunk.Type
        else
            let ip = chunk.Offset
            let signature =
                input.[ip .. ip + 3]
                |> Array.map char
                |> fun s -> String.Join ("", s)
            if signature <> "sfbk" then
                failwithf "invalid signature: %s" signature
            else
                Riff.Parser.parse input (ip + 4 |> Some) (chunk.Size - 4 |> Some) None None
                |> Seq.toArray
                |> function
                    | [| chunk0; chunk1; chunk2 |] ->
                        let samplingData = parseSdtaList input chunk1
                        { Info = parseInfoList input chunk0
                          SamplingData = samplingData
                          PresetData = parsePdtaList input samplingData chunk2 }
                    | _ -> failwith "invalid sfbk structure"
    let parse input ip length padding bigEndian =
        Riff.Parser.parse input ip length padding bigEndian
        |> Seq.exactlyOne
        |> parseRiffChunk input
        
    [<NoComparison>]
    type ModGen =
      { Unknown : ModulatorOrGenerator array
        Lookup : IDictionary<string, ModulatorOrGenerator> }
    [<NoComparison>]
    type ZoneInfo =
      { Generator : ModGen
        GeneratorSequence : ModulatorOrGenerator array
        Modulator : ModGen
        ModulatorSequence : ModulatorOrGenerator array }
    [<NoComparison>]
    type InstrumentZoneInfo =
      { InstrumentName : string
        ZoneInfo : ZoneInfo array }
    [<NoComparison>]
    type PresetZoneInfo =
      { Name : string
        Info : ZoneInfo array
        Header : PresetHeader
        Instrument : int }
    let createBagModGen_ _zone indexStart indexEnd (zoneModGen:ModulatorOrGenerator array) =
        let modgenInfo = zoneModGen.[indexStart .. indexEnd - 1]
        let (unknown, modgen) =
            modgenInfo
            |> Array.partition
                (fun info -> info.Type = null || info.Type = "unknown")
        let modgen =
          { Unknown = unknown
            Lookup =
                modgen
                |> Seq.map (fun info -> (info.Type, info))
                |> Seq.append (Seq.singleton ("keyRange", { Type = "keyRange"; Code = 0us; Amount = 0; Lo = 0uy; Hi = 127uy}))
                |> dict }
        (modgen, modgenInfo)
    let createInstrumentGenerator_ (zone:InstrumentZone array) index (this:Preset) =
        createBagModGen_
            zone
            zone.[index].InstrumentGeneratorIndex
            (if index + 1 < zone.Length then zone.[index + 1].InstrumentGeneratorIndex else this.InstrumentZoneGenerator.Length)
            this.InstrumentZoneGenerator
    let createInstrumentModulator_ (zone:InstrumentZone array) index (this:Preset) =
        createBagModGen_
            zone
            zone.[index].InstrumentModulatorIndex
            (if index + 1 < zone.Length then zone.[index + 1].InstrumentModulatorIndex else this.InstrumentZoneModulator.Length)
            this.InstrumentZoneModulator
    let createPresetGenerator_ (zone:PresetZone array) index (this:Preset) =
        createBagModGen_
            zone
            zone.[index].PresetGeneratorIndex
            (if index + 1 < zone.Length then zone.[index + 1].PresetGeneratorIndex else this.PresetZoneGenerator.Length)
            this.PresetZoneGenerator
    let createPresetModulator_ (zone:PresetZone array) index (this:Preset) =
        createBagModGen_
            zone
            zone.[index].PresetModulatorIndex
            (if index + 1 < zone.Length then zone.[index + 1].PresetModulatorIndex else this.PresetZoneModulator.Length)
            this.PresetZoneModulator
    let getInstruments (this:Preset) =
        let instrument = this.Instruments
        let zone = this.InstrumentZones
        // instrument -> instrument bag -> generator / modulator
        seq {
            for i in 0 .. instrument.Length - 1 do
                let bagIndex = instrument.[i].InstrumentBagIndex
                let bagIndexEnd = if i + 1 < instrument.Length then instrument.[i + 1].InstrumentBagIndex else zone.Length
                // instrument bag
                let zoneInfo =
                    seq {
                        for j in bagIndex .. bagIndexEnd - 1 do
                            let instrumentGenerator = createInstrumentGenerator_ zone j this
                            let instrumentModulator = createInstrumentModulator_ zone j this
                            yield
                              { Generator = fst instrumentGenerator
                                GeneratorSequence = snd instrumentGenerator
                                Modulator = fst instrumentModulator
                                ModulatorSequence = snd instrumentModulator }
                    }
                    |> Seq.toArray
                yield
                  { InstrumentName = instrument.[i].InstrumentName
                    ZoneInfo = zoneInfo }
        }
        |> Seq.toArray
    let getPresets (this:Preset) =
        let preset = this.PresetHeaders
        let zone = this.PresetZones
        // preset -> preset bag -> generator / modulator
        seq {
            for i in 0 .. preset.Length - 1 do
                let bagIndex = preset.[i].PresetBagIndex
                let bagIndexEnd = if i + 1 < preset.Length then preset.[i + 1].PresetBagIndex else zone.Length
                // preset bag
                let zoneInfo =
                    seq {
                        for j in bagIndex .. bagIndexEnd - 1 do
                            let presetGenerator = createPresetGenerator_ zone j this
                            let presetModulator = createPresetModulator_ zone j this
                            yield
                              { Generator = fst presetGenerator
                                GeneratorSequence = snd presetGenerator
                                Modulator = fst presetModulator
                                ModulatorSequence = snd presetModulator }
                    }
                    |> Seq.toArray
                yield
                  { Name = preset.[i].PresetName
                    Info = zoneInfo
                    Header = preset.[i]
                    Instrument =
                        zoneInfo
                        |> Seq.collect
                            (fun i ->
                                [ i.Generator.Lookup.TryGetValue "instrument"
                                  i.Modulator.Lookup.TryGetValue "instrument" ])
                        |> Seq.choose
                            (fun (found, value) -> if found then Some value.Amount else None)
                        |> Seq.tryHead
                        |> function
                            | Some amount -> amount
                            | None -> -1 }
        }
        |> Seq.toArray
