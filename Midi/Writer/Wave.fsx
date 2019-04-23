#r @"F:\Users\Charles\Desktop\fs-game\Midi\Midi\bin\Debug\Midi.dll"
open Midi
open System
open System.IO

let seconds = 4
let sf2 =
    let path = @"F:\Users\Charles\Desktop\FluidR3_GM.SF2"
    use file = new FileStream (path, FileMode.Open)
    let input =
        seq {
            let mutable i = file.ReadByte ()
            while i <> -1 do
                yield byte i
                i <- file.ReadByte ()
        }
        |> Seq.toArray
    SoundFont2.Parser.parse input None None None None
let guitar =
    sf2.Pdta.SampleHeaders
    |> Seq.find
        (fun (_data, header) ->
            header.SampleName.StartsWith "Steel A5")
let sampleRate = (snd guitar).SampleRate

let pack (d:int16[]) = 
    let stream = new MemoryStream ()
    let writer = new BinaryWriter (stream, Text.Encoding.ASCII)
    let dataLength = Array.length d * 2

    // RIFF
    writer.Write (Text.Encoding.ASCII.GetBytes "RIFF")
    writer.Write (Array.length d)
    writer.Write (Text.Encoding.ASCII.GetBytes "WAVE")

    // fmt
    writer.Write (Text.Encoding.ASCII.GetBytes "fmt ")
    writer.Write 16
    writer.Write 1s                     // PCM
    writer.Write 1s                     // mono
    writer.Write sampleRate             // sample rate
    writer.Write (sampleRate * 16 / 8)  // byte rate
    writer.Write 2s                     // bytes per sample
    writer.Write 16s                    // bits per sample

    // data
    writer.Write (Text.Encoding.ASCII.GetBytes "data")
    writer.Write dataLength
    let data = Array.zeroCreate<byte> dataLength
    Buffer.BlockCopy (d, 0, data, 0, data.Length)
    writer.Write data
    stream

//let frequency = 440.0
let getSample =
    let sample = fst guitar
    let loopStart = (snd guitar).StartLoop
    let loopEnd = (snd guitar).EndLoop
    let loopLength = loopEnd - loopStart
    fun index ->
        if index < loopStart then
            sample.[index]
        else
            let index = (index - loopStart) % loopLength
            sample.[index + loopStart]
let samples =
    seq { 0 .. (seconds * sampleRate) - 1 }
    |> Seq.map
        (fun x ->
            x
            |> getSample
            |> int16
            |> max -10000s
            |> min 10000s)

let write (ms:MemoryStream) =
    use fs = new FileStream (Path.Combine (Environment.GetFolderPath Environment.SpecialFolder.Desktop, "test.wav"), FileMode.Create)
    ms.WriteTo fs

samples |> Seq.toArray |> pack |> write
