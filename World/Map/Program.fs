open PlateTectonics
open System
open Util
open Util.Geometry

let debug = Diagnostics.Debug.Write
let debugn = Diagnostics.Debug.WriteLine

[<EntryPoint>]
let main _ =
    let seed = 1
    let height = 80
    let width = 80

    // Plate Tectonics
    let plateCount = 26
    let pNext = 0.3
    let pctLand = 0.4
    let pIsland = 0.05
    let voidChunkSize = 2

    // Wind
    let maxVoid = 2
    let windCenters = 5

    // Temperature
    let minTemp = -10.0
    let maxTemp = 30.0
    let tempSimplexLayers = 3
    let tempSimplexScale = 1.0 / 200.0
    let tempSimplexOffset = 10.0
    let foothillTempDecrease = -5.0
    let oceanCurrent = 0.2

    // Precipitation
    let rain = 0.1
    let maxRainPerSquare = 20.0
    let minRain = 0.0
    let maxRain = 420.0

    let rng = Random seed
    let map = makeLand rng height width plateCount pNext pctLand pIsland voidChunkSize maxVoid

    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            match map.[i, j] with
            | Void -> " "
            | Ocean -> "~"
            | Land -> "+"
            | Foothill -> "n"
            | Mountain -> "A"
            |> debug
        debugn ""

    let wind = Wind.generateWind rng height width windCenters
    debugn ""
    debugn ""
    let angles =
      [ (Vec 1.0 0.0, ">")
        (Vec 1.0 1.0, " ")
        (Vec 0.0 1.0, "^")
        (Vec -1.0 1.0, " ")
        (Vec -1.0 0.0, "<")
        (Vec -1.0 -1.0, " ")
        (Vec 0.0 -1.0, "v")
        (Vec 1.0 -1.0, " ") ]
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            angles
            |> List.minBy (fst >> Vector.angleTo wind.[i, j] >> abs)
            |> snd
            |> debug
        debugn ""

    let temperature =
        map
        |> Temperature.withLand (rng.NextDouble () * 10.0) tempSimplexLayers tempSimplexScale height width tempSimplexOffset foothillTempDecrease oceanCurrent minTemp maxTemp
    let temp =
        temperature
        |> Array2D.map (fun t -> (t - minTemp) / (maxTemp - minTemp) * 10.0 |> int |> min 9)
    debugn ""
    debugn ""
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            temp.[i, j] |> sprintf "%i" |> debug
        debugn ""

    let rain =
        Precipitation.calculate rain maxRainPerSquare minRain maxRain map temperature wind
    let rain0 =
        rain
        |> Array2D.map (fun t -> (t - minRain) / (maxRain - minRain) * 10.0 |> int |> min 9)
    debugn ""
    debugn ""
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            match map.[i, j] with
            | Ocean -> "~"
            | Void -> " "
            | _ -> rain0.[i, j] |> sprintf "%i"
            |> debug
        debugn ""

    0
