module PlateTectonics

open System
open System.Collections.Generic
open Util

type PlateLevel =
    | Oceanic
    | Continental

type Plate =
  { Id : int
    Level : PlateLevel }

type PlateBoundary =
    | Convergent
    | Divergent
    | Transverse

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PlateBoundary =
    let random (rng:Random) =
        match rng.Next 3 with
        | 0 -> Convergent
        | 1 -> Divergent
        | _ -> Transverse

type Land =
    | Void
    | Ocean
    | Land
    | Foothill
    | Mountain

let private formPlates (rng:Random) height width plateCount pNext =
    let queue = Queue<(int * int)> ()
    let plate = Array2D.create height width -1
    for i in 1 .. plateCount do
        let x = rng.Next height
        let y = rng.Next width
        if plate.[x, y] = -1 then
            plate.[x, y] <- i - 1
            queue.Enqueue (x, y)
    let dx = [| 0; 1; 0; -1 |]
    let dy = [| 1; 0; -1; 0 |]
    while queue.Count > 0 do
        let (x, y) = queue.Dequeue ()
        if rng.NextDouble () <= pNext then
            for i in 0 .. 3 do
                let x1 = x + dx.[i]
                let y1 = y + dy.[i]
                if x1 >= 0 && x1 < height && y1 >= 0 && y1 < width && plate.[x1, y1] = -1 then
                    plate.[x1, y1] <- plate.[x, y]
                    queue.Enqueue (x1, y1)
        else
            queue.Enqueue (x, y)
    plate
        
let private setLevels rng pctLand plates =
    let count =
        plates
        |> Array2D.flatten
        |> Seq.countBy id
        |> Seq.randomizeOrder rng
        |> Seq.toList
    let targetLand = Array2D.length1 plates * Array2D.length2 plates |> float |> (*) pctLand |> int
    let rec dividePlates landPlates oceanPlates =
        if Seq.sumBy snd landPlates >= targetLand then
            (List.map fst landPlates, List.map (fun (p, _) -> { Id = p; Level = Oceanic }) oceanPlates)
        else
            match oceanPlates with
            | [] -> (List.map fst landPlates, [])
            | (p, c) :: oceanPlates ->
                dividePlates (({ Id = p; Level = Continental }, c) :: landPlates) oceanPlates
    let (landPlates, oceanPlates) = dividePlates [] count
    landPlates @ oceanPlates

let private findBoundaries rng map =
    let height = Array2D.length1 map
    let width = Array2D.length2 map
    seq {
        for i in 0 .. height - 1 do
            for j in 0 .. width - 1 do
                if i < height - 1 then
                    yield (map.[i, j], map.[i + 1, j])
                if j < width - 1 then
                    yield (map.[i, j], map.[i, j + 1])
    }
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map (fun (a, b) -> (min a b, max a b))
    |> Seq.distinct
    |> Seq.toList
    |> List.map (fun b -> (b, PlateBoundary.random rng))

let private buildLand rng pIsland plates map =
    let height = Array2D.length1 map
    let width = Array2D.length2 map
    let getLevel n = (List.find (fun p -> p.Id = n) plates).Level
    let boundaries = findBoundaries rng map
    let boundaryType a b =
        let (a, b) = (min a b, max a b)
        boundaries
        |> List.pick
            (fun (n, boundary) ->
                if n = (a, b) then Some boundary
                else None)
    let landType = Array2D.map (fun _ -> Void) map
    let tryAssign x y L =
        if x >= 0 && x < height && y >= 0 && y < width && landType.[x, y] = Void then
            landType.[x, y] <- L
    let assignBoundary (x1, y1) (x2, y2) =
        let p1 = map.[x1, y1]
        let p2 = map.[x2, y2]
        if p1 <> p2 then
            match boundaryType p1 p2 with
            | Convergent ->
                match (getLevel p1, getLevel p2) with
                | (Oceanic, Oceanic) ->
                    if rng.NextDouble () <= pIsland then
                        tryAssign x1 y1 Mountain
                    if rng.NextDouble () <= pIsland then
                        tryAssign x2 y2 Mountain
                | (Oceanic, Continental) ->
                    tryAssign x2 y2 Mountain
                | (Continental, Oceanic) ->
                    tryAssign x1 y1 Mountain
                | (Continental, Continental) ->
                    tryAssign x1 y1 Mountain
                    tryAssign x2 y2 Mountain
            | Divergent | Transverse -> ()
    Array2D.iteri
        (fun i j _ ->
            if i < height - 1 then
                assignBoundary (i, j) (i + 1, j)
            if j < width - 1 then
                assignBoundary (i, j) (i, j + 1))
        map
    Array2D.iteri
        (fun i j t ->
            if t = Mountain then
                for x in i - 1 .. i + 1 do
                    for y in j - 1 .. j + 1 do
                        tryAssign x y Foothill)
        landType
    landType
    |> Array2D.mapi
        (fun i j t ->
            if t = Void then
                match getLevel map.[i, j] with
                | Oceanic -> Ocean
                | Continental -> Land
            else t)

let private addVoid (rng:Random) chunkSize maxVoid map =
    let map = Array2D.copy map
    let next n =
        rng.Next 3 - 1 + n
        |> max 0
        |> min maxVoid
    let makeEdge () =
        Seq.unfold
            (fun (n, c) ->
                if c = 0 then
                    let n = next n
                    Some (n, (n, chunkSize - 1))
                else
                    Some (n, (n, c - 1)))
            (rng.Next (maxVoid + 1), chunkSize)
    let setVoid (i, j) = map.[i, j] <- Void
    let doEdge length mapIndex =
        let edge =
            makeEdge ()
            |> Seq.take length
            |> Seq.toList
        edge
        |> List.iteri
            (fun index v ->
                for i in 0 .. v - 1 do
                    mapIndex index i |> setVoid)
    let height = Array2D.length1 map
    let width = Array2D.length2 map
    doEdge height (fun index n -> (index, n))
    doEdge height (fun index n -> (index, width - 1 - n))
    doEdge width (fun index n -> (n, index))
    doEdge width (fun index n -> (height - 1 - n, index))
    map

let makeLand rng height width plateCount pNext pctLand pIsland voidChunkSize maxVoid =
    let map = formPlates rng height width plateCount pNext
    let plates = setLevels rng pctLand map
    let map = buildLand rng pIsland plates map
    let map = addVoid rng voidChunkSize maxVoid map
    map