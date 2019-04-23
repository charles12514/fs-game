open System
open System.Collections.Generic
open System.ComponentModel
open System.Drawing
open System.IO
open Util

[<NoComparison; NoEquality>]
type PatternFactory<'a> =
    private
        { MakePattern : int -> int -> 'a[,] -> 'a[,]
          Encode : 'a[,] -> int
          Decode : int -> 'a[,] }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PatternFactory =
    let make size states =
        let states = Seq.distinct states
        let toInt =
            states
            |> Seq.mapi (fun i x -> (x, i))
            |> Map.ofSeq
        let toState =
            states
            |> Seq.mapi (fun i x -> (i, x))
            |> Map.ofSeq
        let bits =
            let mutable bits = 1
            while (1 <<< bits) < toInt.Count do
                bits <- bits + 1
            bits
        let encode =
            Array2D.fold
                (fun state data ->
                    match Map.tryFind data toInt with
                    | Some i -> (state <<< bits) ||| i
                    | None -> failwithf "Unknown state %A" data)
                0
        let decode index =
            let mutable index = index
            let mask = (1 <<< bits) - 1
            let array = Array2D.zeroCreate size size
            for i in size - 1 .. -1 .. 0 do
                for j in size - 1 .. -1 .. 0 do
                    let x = index &&& mask
                    array.[i, j] <- toState.[x]
                    index <- index >>> bits
            array
        let patternAt x y array =
            let L1 = Array2D.length1 array
            let L2 = Array2D.length2 array
            let get x y = Array2D.get array <| (x + L1) % L1 <| (y + L2) % L2
            Array2D.init size size
                (fun i j -> get (x + i) (y + j))
        { MakePattern = patternAt
          Encode = encode
          Decode = decode }

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

let mcmc (random:Random) sample n sizeW sizeH iterations getRotationsAndReflections getProbability =
    let states =
        sample
        |> Array2D.flatten
        |> Seq.distinct
        |> Seq.toList
    let factory = PatternFactory.make n states

    // create markov chain
    let chain =
        seq {
            for x in 0 .. Array2D.length1 sample - 1 do
                for y in 0 .. Array2D.length2 sample - 1 do
                    yield!
                        sample
                        |> factory.MakePattern x y
                        |> getRotationsAndReflections
                        |> Seq.map factory.Encode
        }
        |> MarkovChain.make

    // repeat monte carlo sampling

    // create a random start state
    let field = Array2D.init sizeW sizeH (fun _ _ -> states |> List.length |> random.Next |> List.item <| states)

    // combined weight of all windows that include (i, j) when (i, j) = color
    // return value <= 1
    let energy color i j =
        let oldColor = field.[i, j]
        field.[i, j] <- color

        let value =
            seq {
                for x in i - n + 1 .. i do
                    for y in j - n + 1 .. j do
                        let index =
                            field
                            |> factory.MakePattern x y
                            |> factory.Encode
                        yield MarkovChain.weight index chain
            }
            |> Seq.sum

        field.[i, j] <- oldColor
        value

    // set (i, j), weighted by energy
    // lower temperature -> closer to picking highest weight (more similar to sample, less random)
    let heatBath i j =
        let color =
            states
            |> Seq.weightedRandom
                random
                (fun color -> energy color i j |> getProbability)
        field.[i, j] <- color

    // pick a random pixel and apply heatBath
    for _ in 1 .. iterations * sizeW * sizeH do
        heatBath <| random.Next sizeW <| random.Next sizeH

    field

module Neighborhood =
    let private rotate array =
        let L1 = Array2D.length1 array
        Array2D.init
            (Array2D.length2 array)
            L1
            (fun x y -> array.[L1 - 1 - y, x])
    let private reflect array =
        let L1 = Array2D.length1 array
        Array2D.init
            L1
            (Array2D.length2 array)
            (fun x y -> array.[L1 - 1 - x, y])
    let rotateAndReflect p0 =
        let p1 = p0 |> rotate
        let p2 = p1 |> rotate
        let p3 = p2 |> rotate
        let p4 = p0 |> reflect
        let p5 = p1 |> reflect
        let p6 = p2 |> reflect
        let p7 = p3 |> reflect
        [ p0; p1; p2; p3; p4; p5; p6; p7 ]
    let reflectOnly p0 =
        [ p0; reflect p0 ]
    let id p = [ p ]

module HeatBath =
    let probability temperature energy =
        energy / temperature |> Math.Exp

let bitmapToArray (bitmap:Bitmap) =
    Array2D.init
        bitmap.Width
        bitmap.Height
        (fun i j ->
            let c = bitmap.GetPixel (i, j)
            (c.R, c.G, c.B))

let inline arrayToBitmap array =
    let bitmap = new Bitmap (Array2D.length1 array, Array2D.length2 array)
    array
    |> Array2D.iteri
        (fun x y (r, g, b) ->
            bitmap.SetPixel (x, y, Color.FromArgb (255, int r, int g, int b)))
    bitmap

[<EntryPoint>]
let main _ =
    let desktop = Environment.SpecialFolder.Desktop |> Environment.GetFolderPath
    let input = Path.Combine (desktop, "in.bmp")
    let output = Path.Combine (desktop, "out.bmp")
    let seed = 1

    let sample =
        new Bitmap (input)
        |> bitmapToArray
    let result =
        mcmc
            (Random seed)
            sample
            2
            50
            50
            5
            Neighborhood.id
            (HeatBath.probability 0.01)
        |> arrayToBitmap
    result.Save output

    0
