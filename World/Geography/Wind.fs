module Wind

open Util
open Util.Geometry

let generateWind (rng:System.Random) size1 size2 centers =
    let add x0 y0 clockwise strength =
        Array2D.mapi
            (fun y x v ->
                let toPoint = Vec (x - x0 |> float) (y - y0 |> float)
                if Vector.mag2 toPoint |> gt0 then
                    let magnitude = strength / Vector.mag2 toPoint
                    let dir =
                        let dir = toPoint |> Vector.perpendicular |> Vector.ofLength magnitude
                        if clockwise then -dir
                        else dir
                    v + dir
                else v)
    let wind =
        Util.iterate
            (fun wind ->
                let x = rng.Next (size1 * 2) - (size1 / 2)
                let y = rng.Next (size2 * 2) - (size2 / 2)
                let clockwise = rng.Next 2 = 0
                let strength = rng.Next 20 |> (+) 4 |> (*) 100 |> float
                add x y clockwise strength wind)
            centers
            (Array2D.create size1 size2 Vector.Zero)
    for i in 0 .. size1 / 2 - 1 do
        for j in 0 .. size2 - 1 do
            let a = wind.[i, j]
            wind.[i, j] <- wind.[size1 - 1 - i, j]
            wind.[size1 - 1 - i, j] <- a
    wind
