module Biome

open PlateTectonics

// https://en.wikipedia.org/wiki/Biome#/media/File:PrecipitationTempBiomes.jpg
// 10 deg C per 1000 m

type Biome =
    | Ocean
    | Void
    | TropicalRainforest
    | TropicalSeasonalForestSavanna
    | SubtropicalDesert
    | TemperateRainforest
    | TemperateSeasonalForest
    | WoodlandShrubland
    | TemperateGrasslandColdDesert
    | BorealForest
    | Tundra

type BiomeInfo =
  { Biome : Biome
    MinTemperature : int
    MaxTemperature : int
    MinRainfall : int
    MaxRainfall : int }

module DefaultBiomeInfo =
    let tropicalRainforest =
      { Biome = TropicalRainforest
        MinTemperature = 20
        MaxTemperature = 30
        MinRainfall = 250
        MaxRainfall = 420 }
    let tropicalSeasonalForestSavanna =
      { Biome = TropicalSeasonalForestSavanna
        MinTemperature = 20
        MaxTemperature = 30
        MinRainfall = 80
        MaxRainfall = 250 }
    let subtropicalDesert =
      { Biome = SubtropicalDesert
        MinTemperature = 20
        MaxTemperature = 30
        MinRainfall = 0
        MaxRainfall = 80 }
    let temperateRainforest =
      { Biome = TemperateRainforest
        MinTemperature = 6
        MaxTemperature = 20
        MinRainfall = 200
        MaxRainfall = 300 }
    let temperateSeasonalForest =
      { Biome = TemperateSeasonalForest
        MinTemperature = 6
        MaxTemperature = 20
        MinRainfall = 80
        MaxRainfall = 200 }
    let woodlandShrubland =
      { Biome = WoodlandShrubland
        MinTemperature = 0
        MaxTemperature = 20
        MinRainfall = 40
        MaxRainfall = 80 }
    let temperateGrasslandColdDesert =
      { Biome = TemperateGrasslandColdDesert
        MinTemperature = 0
        MaxTemperature = 20
        MinRainfall = 0
        MaxRainfall = 40 }
    let borealForest =
      { Biome = BorealForest
        MinTemperature = 0
        MaxTemperature = 6
        MinRainfall = 80
        MaxRainfall = 200 }
    let tundra =
      { Biome = Tundra
        MinTemperature = -10
        MaxTemperature = 0
        MinRainfall = 0
        MaxRainfall = 100 }
    let all =
      [ tropicalRainforest
        tropicalSeasonalForestSavanna
        subtropicalDesert
        temperateRainforest
        temperateSeasonalForest
        woodlandShrubland
        temperateGrasslandColdDesert
        borealForest
        tundra ]
    let tryFind temperature rainfall =
        all
        |> List.tryFind
            (fun biome ->
                biome.MinTemperature <= temperature &&
                biome.MaxTemperature >= temperature &&
                biome.MinRainfall <= rainfall &&
                biome.MaxRainfall >= rainfall)
        |> Option.map (fun biome -> biome.Biome)
    let best temperature rainfall =
        tryFind temperature rainfall
        |> function
            | Some biome -> biome
            | None ->
                all
                |> List.minBy
                    (fun biome ->
                        let tempDiff =
                            if temperature < biome.MinTemperature then biome.MinTemperature - temperature
                            elif temperature > biome.MaxTemperature then temperature - biome.MaxTemperature
                            else 0
                        let rainDiff =
                            if rainfall < biome.MinRainfall then biome.MinRainfall - rainfall
                            elif rainfall > biome.MaxRainfall then rainfall - biome.MaxRainfall
                            else 0
                        tempDiff * tempDiff + rainDiff * rainDiff)
                |> fun b -> b.Biome
                
let classify map (temperature:float[,]) (precipitation:float[,]) =
    map
    |> Array2D.mapi
        (fun i j ->
            function
            | Land | Foothill | Mountain ->
                DefaultBiomeInfo.best (int temperature.[i, j]) (int precipitation.[i, j])
            | PlateTectonics.Ocean -> Ocean
            | PlateTectonics.Void -> Void)
