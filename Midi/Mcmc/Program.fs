open System
open System.Collections.Generic
open System.ComponentModel
open System.Drawing
open System.IO
open System.Xml
open Util

type Pattern =
    private
        { Data : bool[,] }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Pattern =
    let size pattern = pattern.Data.GetLength 0
    let make f size =
        { Data = Array2D.init size size f }
    let private posMod x m = (x + m) % m
    let fromData (field:bool[,]) x y =
        make (fun i j -> field.[posMod (x + i) (field.GetLength 0), posMod (y + j) (field.GetLength 1)])
    let rotate pattern =
        let size = size pattern
        make
            (fun x y -> pattern.Data.[size - 1 - y, x])
            size
    let reflect pattern =
        let size = size pattern
        make
            (fun x y -> pattern.Data.[size - 1 - x, y])
            size
    let index pattern =
        pattern.Data
        |> Array2D.fold
            (fun (result, power) value ->
                (result + (if value then 1 else 0) * power, power * 2))
            (0, 1)
        |> fst

[<NoComparison>]
type MarkovChain =
    private
        { Weights : IDictionary<int, float> }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MarkovChain =
    let make samples =
        let weights =
            let weights = Dictionary<int, int> ()
            for i in samples do
                weights.[i] <-
                    match weights.TryGetValue i with
                    | (true, w) -> w
                    | (false, _) -> 0
                    |> (+) 1
            let sum = weights.Values |> Seq.sum
            weights
            |> seq
            |> Seq.map (fun kv -> (kv.Key, float kv.Value / float sum))
            |> dict
        { Weights = weights }
    let weight index markovChain =
        match markovChain.Weights.TryGetValue index with
        | (true, p) -> p
        | (false, _) -> 0.0

let mcmc (random:Random) (sample:bool[,]) N sizeW sizeH iterations getRotationsAndReflections getProbability =
    // create markov chain
    let chain =
        seq {
            for x in 0 .. sample.GetLength 0 - 1 do
                for y in 0 .. sample.GetLength 1 - 1 do
                    yield!
                        Pattern.fromData sample x y N
                        |> getRotationsAndReflections
                        |> Seq.map Pattern.index
        }
        |> MarkovChain.make

    // repeat monte carlo sampling

    // create a random start state
    let field = Array2D.init sizeW sizeH (fun _ _ -> random.Next 2 = 0)

    // combined weight of all windows that include (i, j) when (i, j) = color
    // return value <= 1
    let energy color i j =
        let oldColor = field.[i, j]
        field.[i, j] <- color

        let value =
            seq {
                for x in i - N + 1 .. i + N - 1 do
                    for y in j - N + 1 .. j + N - 1 do
                        let index =
                            Pattern.fromData field x y N
                            |> Pattern.index
                        yield MarkovChain.weight index chain
            }
            |> Seq.sum

        field.[i, j] <- oldColor
        value

    // set (i, j), weighted by energy
    // lower temperature -> closer to picking highest weight (more similar to sample, less random)
    let heatBath i j =
        let color =
            [ false; true ]
            |> Seq.weightedRandom
                random
                (fun color -> energy color i j |> getProbability)
        field.[i, j] <- color

    // pick a random pixel and apply heatBath
    for _ in 1 .. iterations * sizeW * sizeH do
        heatBath <| random.Next sizeW <| random.Next sizeH

    field

let mcmc3d (random:Random) (sample:bool[,][]) N sizeW sizeH iterations getProbability =
    let window = 2 * N + 1
    let get (arr:bool[,]) x y =
        let x = (x + arr.GetLength 0) % arr.GetLength 0
        let y = (y + arr.GetLength 1) % arr.GetLength 1
        arr.[x, y]
    let index (i:bool[,]) j x y =
        // sample i influenced by sample j (pattern from j + pixel from i)
        let p = Pattern.fromData j (x - N) (y - N) window
        (Pattern.index p <<< 1) + (if get i x y then 1 else 0)
        
    // create markov chains
    let matrix =
        Array2D.init
            sample.Length
            sample.Length
            (fun i j ->
                // sample i influenced by sample j
                seq {
                    for x in 0 .. sample.[i].GetLength 0 - 1 do
                        for y in 0 .. sample.[i].GetLength 1 - 1 do
                            yield index sample.[i] sample.[j] x y
                }
                |> MarkovChain.make)

    // repeat monte carlo sampling

    // create a random start state
    let field =
        Array.init sample.Length
            <| fun _ -> Array2D.init sizeW sizeH (fun _ _ -> random.Next 2 = 0)

    // combined weight of all windows that include (i, j) when (i, j) = color
    // return value <= 1
    let energy color s i j =
        let oldColor = field.[s].[i, j]
        field.[s].[i, j] <- color

        let value =
            seq {
                for t in 0 .. field.Length - 1 do
                    for x in i - window .. i do
                        for y in j - window .. j do
                            let index = index field.[t] field.[s] x y
                            let w = MarkovChain.weight index matrix.[s, t]
                            yield if s = t then w * float sample.Length else w
            }
            |> Seq.sum

        field.[s].[i, j] <- oldColor
        value

    // set (i, j), weighted by energy
    // lower temperature -> closer to picking highest weight (more similar to sample, less random)
    let heatBath s i j =
        let color =
            [ false; true ]
            |> Seq.weightedRandom
                random
                (fun color -> energy color s i j |> getProbability)
        field.[s].[i, j] <- color

    // pick a random pixel and apply heatBath
    for _ in 1 .. iterations * field.Length * sizeW * sizeH do
        heatBath <| random.Next field.Length <| random.Next sizeW <| random.Next sizeH

    field

module Default =
    let rotateAndReflect p0 =
        let p1 = p0 |> Pattern.rotate
        let p2 = p1 |> Pattern.rotate
        let p3 = p2 |> Pattern.rotate
        let p4 = p0 |> Pattern.reflect
        let p5 = p1 |> Pattern.reflect
        let p6 = p2 |> Pattern.reflect
        let p7 = p3 |> Pattern.reflect
        [ p0; p1; p2; p3; p4; p5; p6; p7 ]
    let probability temperature energy =
        energy / temperature |> Math.Exp
        
let attrOrDefault (node:XmlNode) attribute def =
    let s = (node :?> XmlElement).GetAttribute attribute
    if s = "" then def
    else
        let converter = TypeDescriptor.GetConverter typeof<'a>
        converter.ConvertFromString s :?> 'a

let bitmapToData (bitmap:Bitmap) =
    let get i j =
        let pix = bitmap.GetPixel (i, j)
        pix.R
    Array2D.init
        bitmap.Width
        bitmap.Height
        (fun i j -> get i j = 0uy)

let dataToBitmap (data:bool[,]) =
    let bitmap = new Bitmap (data.GetLength 0, data.GetLength 1)
    data
    |> Array2D.iteri
        (fun x y c ->
            bitmap.SetPixel (x, y, if c then Color.Black else Color.LightGray))
    bitmap

let runSamples seed =
    let desktop = Environment.SpecialFolder.Desktop |> Environment.GetFolderPath
    let xdoc = new XmlDocument ()
    Path.Combine (desktop, "samples.xml")
    |> xdoc.Load

    let rec processSamples (xnode:XmlNode) =
        if xnode <> null then
            for k in 1 .. attrOrDefault xnode "screenshots" 1 do
                let name = attrOrDefault xnode "name" ""
                let random = Random seed
                let output =
                    let data =
                        new Bitmap (Path.Combine (desktop, "Samples", name + ".bmp"))
                        |> bitmapToData
                    mcmc
                        <| random
                        <| data
                        <| attrOrDefault xnode "receptorSize" 2
                        <| (attrOrDefault xnode "outputSize" 32) * 2
                        <| attrOrDefault xnode "outputSize" 32
                        <| attrOrDefault xnode "iterations" 2
                        <| Default.rotateAndReflect
                        <| Default.probability (attrOrDefault xnode "temperature" 0.02)
                Path.Combine (desktop, sprintf "%s %i.bmp" name (k - 1))
                |> (output |> dataToBitmap).Save
            processSamples xnode.NextSibling
    processSamples xdoc.FirstChild.FirstChild

let run3d seed =
    let desktop = Environment.SpecialFolder.Desktop |> Environment.GetFolderPath
    let random = Random seed
    let output =
        let data =
            [|  "Bass"
                "Melody 1"
                "Melody 2"
                "Melody 3" |]
            |> Array.map
                (fun name ->
                    new Bitmap (Path.Combine (desktop, "Samples", name + ".bmp"))
                    |> bitmapToData)
        mcmc3d
            <| random
            <| data
            <| 1
            <| 64
            <| 64
            <| 8
            <| Default.probability 0.01
    output
    |> Seq.map dataToBitmap
    |> Seq.iteri
        (fun k bmp ->
            Path.Combine (desktop, sprintf "3d %i.bmp" k)
            |> bmp.Save)

[<EntryPoint>]
let main _ =
    let seed = 0
    runSamples seed

    0
