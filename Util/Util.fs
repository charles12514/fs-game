namespace Util

module Constants =
    [<Literal>]
    let E = 1e-6
    let divE = 1.0 / E

    [<Literal>]
    let PI = System.Math.PI

[<RequireQualifiedAccess>]
module Log =
    let private logFile =
        let file = @"log.txt"
        use writer = new System.IO.StreamWriter (file, false)
        writer.Write ""
        file

    let write (text:string) =
        use writer = new System.IO.StreamWriter (logFile, true)
        writer.WriteLine text

[<AutoOpen>]
module Common =
    
    let unreachable () = failwith "unreachable"

    let eq0 x = abs x < Constants.E
    let neq0 = eq0 >> not
    let gt0 x = x >= Constants.E
    let lt0 x = x <= -Constants.E
    let geq0 = lt0 >> not
    let leq0 = gt0 >> not

    let private testE f x y = x - y |> f
    let eqE = testE eq0
    let neqE = testE neq0
    let gtE = testE gt0
    let ltE = testE lt0
    let geqE = testE geq0
    let leqE = testE leq0

module Util =

    let floorE = (*) Constants.divE >> int >> float >> (*) Constants.E
    let sign x =
        if lt0 x then -1
        elif gt0 x then 1
        else 0

    let rec gcd a b = if b = 0 then a else gcd b (a % b)
    let lcm a b = b / gcd a b * a

    let initDictionary keySet value =
        let d = System.Collections.Generic.Dictionary<'k, 'v> ()
        Seq.iter (fun key -> d.[key] <- value) keySet
        d

    let rec iterate action iterations state =
        if iterations <= 0 then state
        else state |> action |> iterate action (iterations - 1)

    let coordinateCompression values =
        let valueAt = values |> Seq.distinct |> Seq.sort |> Seq.toList
        let indexOf = valueAt |> Seq.mapi (fun index value -> value, index) |> dict
        valueAt, indexOf
    
    let mapFst f (a, b) = (f a, b)
    let mapSnd f (a, b) = (a, f b)

type UnionFind<'a when 'a : equality> () =
    let root = System.Collections.Generic.Dictionary<'a, 'a option>()

    member this.find x =
        match root.TryGetValue x with
        | false, _ | true, None -> x
        | true, Some r ->
            let newRoot = this.find r
            root.[x] <- Some newRoot
            newRoot
    member this.union x y =
        let rootX = this.find x
        let rootY = this.find y
        if rootX <> rootY then
            root.[rootY] <- Some rootX

[<RequireQualifiedAccess>]
module Array2D =
    let flatten array =
        seq {
            for i in 0 .. Array2D.length1 array - 1 do
                for j in 0 .. Array2D.length2 array - 1 do
                    yield array.[i, j]
        }
    let fold folder state = flatten >> Seq.fold folder state
    let foldi folder state array =
        let length1 = Array2D.length1 array
        let length2 = Array2D.length2 array
        let rec f i j s =
            if i >= length1 then s
            elif j >= length2 then f (i + 1) 0 s
            else f i <| j + 1 <| folder s i j array.[i, j]
        f 0 0 state
    let inline rescale newMin newMax array =
        let min = array |> fold min array.[0, 0]
        let max = array |> fold max array.[0, 0]
        array
        |> Array2D.map
            (fun x -> (x - min) / (max - min) * (newMax - newMin) + newMin)


[<RequireQualifiedAccess>]
module List =
    let contains element = List.exists ((=) element)
    let foldi folder state list =
        let rec f state index list =
            match list with
            | [] -> state
            | head :: tail -> f <| folder state index head <| index + 1 <| tail
        f state 0 list
    let withoutLastOccurrenceOf value list =
        List.foldBack
            (fun elem (acc, found) ->
                if not found && elem = value then (acc, true)
                else ((elem :: acc), found))
            list
            ([], false)
        |> fst

[<RequireQualifiedAccess>]
module Seq =
    let contains element = Seq.exists ((=) element)
    let randomizeOrder (rng:System.Random) = Seq.sortBy (fun _ -> rng.Next ())
    let weightedRandom (rng:System.Random) weight values =
        let x = values |> Seq.toList |> List.map (fun element -> (element, weight element))
        List.fold
            (fun (rand, chosen) (element, weight) ->
                match chosen with
                | None ->
                    if rand <= weight then (0.0, Some element)
                    else (rand - weight, None)
                | Some value -> (rand, Some value))
            (List.sumBy snd x * rng.NextDouble (), None)
            x
        |> snd
        |> Option.get
    let pickRandom (rng:System.Random) values =
        values
        |> Seq.length
        |> rng.Next
        |> Seq.item <| values
    let rotateLeft n sequence =
        let len = Seq.length sequence
        let n = n % len
        let n = if n < 0 then n + len else n
        Seq.append (Seq.skip n sequence) (Seq.take n sequence)
    let circularPairwise x = x |> rotateLeft 1 |> Seq.zip x

type Attempt<'a, 'b> =
    | Success of 'a
    | Failure of 'b

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Attempt =
    let bind f =
        function
        | Success x -> f x
        | Failure x -> Failure x

    let map f =
        function
        | Success x -> x |> f |> Success
        | Failure x -> Failure x

    let getSuccess attempt =
        match attempt with
        | Success s -> s
        | Failure _ -> invalidArg "this" "Unable to get success value of a failure"

    let getFailure attempt =
        match attempt with
        | Failure f -> f
        | Success _ -> invalidArg "this" "Unable to get failure value of a success"
