module Precipitation

open PlateTectonics
open System.Collections.Generic
open Util
open Util.Geometry

// http://web.gccaz.edu/~lnewman/gph111/topic_units/Labs_all/Water%20Vapor%20Capacity%20of%20Air.pdf
// {-40.0, 0.1},{-35.0, 0.2},{-30.0, 0.3},{-25.0, 0.51},{-20.0, 0.75},{-10.0, 1.8},{0.0, 3.8},{5.0, 5.0},{10.0, 7.8},{15.0, 10.0},{20.0, 15.0},{25.0, 20.0},{30.0, 27.7},{35.0, 35.0},{40.0, 49.8}
// best fit is f(x) = 4.13068 * e^(0.0621211 * x)
// x is degrees C
// f(x) is g vapor / kg air

let calculate rain maxRainPerSquare minRain maxRain map (temperature:float[,]) (wind:Vector[,]) =
    let height = Array2D.length1 map
    let width = Array2D.length2 map
    let vaporCapacity =
        (*) 0.0621211 >> exp >> (*) 4.13068
    let rain temperature vapor =
        let capacity = vaporCapacity temperature
        if vapor <= capacity then
            vapor * rain
        else
            vapor - capacity + capacity * rain
        |> min maxRainPerSquare
    let queue = Queue<int * int> ()
    let vapor =
        Array2D.init
            height
            width
            (fun i j ->
                match map.[i, j] with
                | Ocean ->
                    queue.Enqueue (i, j)
                    temperature.[i, j] |> vaporCapacity
                | _ -> 0.0)
    let precipitation = Array2D.create height width None
    let next = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]
    while queue.Count > 0 do
        let (i, j) = queue.Dequeue ()
        if precipitation.[i, j].IsNone then
            let rain = rain temperature.[i, j] vapor.[i, j]
            vapor.[i, j] <- vapor.[i, j] - rain
            precipitation.[i, j] <-
                if map.[i, j] = Ocean || map.[i, j] = Void then 0.0
                else rain
                |> Some
            for (di, dj) in next do
                let i1 = i + di
                let j1 = j + dj
                if i1 >= 0 && i1 < height && j1 >= 0 && j1 < width then
                    let d = Vec (float dj) (float -di)
                    if d |> Vector.dot wind.[i, j] > 0.0 then
                        vapor.[i1, j1] <-
                            wind.[i, j]
                            |> Vector.angleTo d
                            |> abs
                            |> (*) (2.0 / Constants.PI)
                            |> (-) 1.0
                            |> (*) vapor.[i, j]
                            |> (+) vapor.[i1, j1]
                        queue.Enqueue (i1, j1)
    precipitation
    |> Array2D.map
        (function
         | Some p -> p
         | None -> 0.0)
    |> Array2D.rescale minRain maxRain
