namespace Util

type Treap<'a> =
    | Nil
    | Node of 'a * int * Treap<'a> * Treap<'a>

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Treap =
    let private rng = System.Random ()

    let empty = Nil

    let rec contains value =
        function
        | Nil -> false
        | Node (v, _, left, right) ->
            if value = v then true
            elif value < v then contains value left
            else contains value right
    
    let private tryLeftRotation =
        function
        | Nil -> Nil
        | Node (v1, k1, x, c) as root ->
            match x with
            | Nil -> root
            | Node (v2, k2, a, b) when k2 > k1 -> Node (v2, k2, a, Node (v1, k1, b, c))
            | _ -> root

    let private tryRightRotation =
        function
        | Nil -> Nil
        | Node (v1, k1, a, x) as root ->
            match x with
            | Nil -> root
            | Node (v2, k2, b, c) when k2 > k1 -> Node (v2, k2, Node (v1, k1, a, b), c)
            | _ -> root

    let rec insert value =
        function
        | Nil -> Node (value, rng.Next (), Nil, Nil)
        | Node (v, k, left, right) as root ->
            if value = v then root
            elif value < v then Node (v, k, insert value left, right)
            else Node (v, k, left, insert value right)
            |> tryLeftRotation
            |> tryRightRotation

    let rec min =
        function
        | Nil -> None
        | Node (v, _, Nil, _) -> Some v
        | Node (_, _, left, _) -> min left

    let rec max =
        function
        | Nil -> None
        | Node (v, _, _, Nil) -> Some v
        | Node (_, _, _, right) -> max right
    
    let rec previous value =
        function
        | Nil -> None
        | Node (v, _, left, right) ->
            if value < v then previous value left
            elif v < value then
                match previous value right with
                | Some p -> Some p
                | None -> Some v
            else max left

    let rec next value =
        function
        | Nil -> None
        | Node (v, _, left, right) ->
            if value < v then
                match next value left with
                | Some p -> Some p
                | None -> Some v
            elif v < value then next value right
            else min right

    let rec delete value =
        function
        | Nil -> Nil
        | Node (v, k, left, right) ->
            if value < v then Node (v, k, delete value left, right)
            elif v < value then Node (v, k, left, delete value right)
            else
                if left = Nil then right
                else
                    match next value right with
                    | None -> left
                    | Some succVal -> Node (succVal, k, left, delete succVal right)
