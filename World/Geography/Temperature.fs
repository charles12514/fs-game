module Temperature

open PlateTectonics
open System.Collections.Generic
open Util

let private monopole height width minTemp maxTemp =
    Array2D.init height width (fun y _ -> float y / float height)
    |> Array2D.rescale minTemp maxTemp

let private simplex seed layers scale height width minTemp maxTemp =
    let simplex = Simplex.create seed layers scale
    Array2D.init height width
        (fun y x -> simplex <| float x <| float y)
    |> Array2D.rescale minTemp maxTemp

let private independent seed layers scale height width simplexOffset minTemp maxTemp =
    let pole = monopole height width minTemp maxTemp
    let simplex = simplex seed layers scale height width -simplexOffset simplexOffset
    Array2D.init height width (fun i j -> pole.[i, j] + simplex.[i, j])
    |> Array2D.rescale minTemp maxTemp

let withLand seed layers scale height width simplexOffset tempFalloff oceanCurrent minTemp maxTemp map =
    let cooledMountains =
        let hillDist = Array2D.create height width -1
        let queue = Queue<int * int> ()
        map
        |> Array2D.iteri
            (fun i j ->
                function
                | Void | Ocean | Land ->
                    hillDist.[i, j] <- 0
                    queue.Enqueue (i, j)
                | Foothill | Mountain -> ())
        let d =
            [| (0, 1)
               (1, 0)
               (0, -1)
               (-1, 0) |]
        while queue.Count > 0 do
            let (i, j) = queue.Dequeue ()
            for (di, dj) in d do
                let i1 = i + di
                let j1 = j + dj
                if i1 >= 0 && i1 < height && j1 >= 0 && j1 < width && hillDist.[i1, j1] = -1 then
                    hillDist.[i1, j1] <- hillDist.[i, j] + 1
                    queue.Enqueue (i1, j1)
        independent seed layers scale height width simplexOffset minTemp maxTemp
        |> Array2D.mapi (fun i j -> (+) (float hillDist.[i, j] * tempFalloff) >> max minTemp)
    let oceanAvg =
        cooledMountains
        |> Array2D.mapi (fun i j t -> (map.[i, j], t))
        |> Array2D.flatten
        |> Seq.choose
            (function
                | (Ocean, t) | (Void, t) -> Some t
                | _ -> None)
        |> Seq.average
    cooledMountains
    |> Array2D.mapi
        (fun i j t ->
            match map.[i, j] with
            | Ocean | Void -> t * (1.0 - oceanCurrent) + oceanAvg * oceanCurrent
            | Land | Foothill | Mountain -> t)
    |> Array2D.rescale minTemp maxTemp
