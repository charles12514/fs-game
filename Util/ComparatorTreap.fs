namespace Util

[<RequireQualifiedAccess>]
module ComparatorTreap =
    let private rng = System.Random ()

    let empty = Nil

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
    
    let rec contains comparator value =
        function
        | Nil -> false
        | Node (v, _, left, right) ->
            if comparator value v = 0 then true
            elif comparator value v < 0 then contains comparator value left
            else contains comparator value right

    let rec insert comparator value =
        function
        | Nil -> Node (value, rng.Next (), Nil, Nil)
        | Node (v, k, left, right) as root ->
            if comparator value v = 0 then root
            elif comparator value v < 0 then Node (v, k, insert comparator value left, right)
            else Node (v, k, left, insert comparator value right)
            |> tryLeftRotation
            |> tryRightRotation

    let rec previous comparator value =
        function
        | Nil -> None
        | Node (v, _, left, right) ->
            if comparator value v < 0 then previous comparator value left
            elif comparator v value < 0 then
                match previous comparator value right with
                | Some p -> Some p
                | None -> Some v
            else max left

    let rec next comparator value =
        function
        | Nil -> None
        | Node (v, _, left, right) ->
            if comparator value v < 0 then
                match next comparator value left with
                | Some p -> Some p
                | None -> Some v
            elif comparator v value < 0 then next comparator value right
            else min right

    let rec delete comparator value =
        function
        | Nil -> Nil
        | Node (v, k, left, right) ->
            if comparator value v < 0 then Node (v, k, delete comparator value left, right)
            elif comparator v value < 0 then Node (v, k, left, delete comparator value right)
            else
                match left with
                | Nil -> right
                | _ ->
                    match next comparator value right with
                    | None -> left
                    | Some succVal -> Node (succVal, k, left, delete comparator succVal right)
