namespace Util

type Matrix<'a> =
    private { Values : 'a[,] }
    member this.Item (i, j) = this.Values.[i, j]
    member this.Row i = this.Values.[i, *]
    member this.Col i = this.Values.[*, i]
    member this.Rows = Array2D.length1 this.Values
    member this.Cols = Array2D.length2 this.Values
    override this.ToString () =
        seq {
            for i in 0 .. this.Rows - 1 do
                yield if i = 0 then "[" else " "
                for j in 0 .. this.Cols - 1 do
                    yield sprintf " %A" this.Values.[i, j]
                yield if i = this.Rows - 1 then " ]" else System.Environment.NewLine
        }
        |> fun s -> System.String.Join ("", s)

[<RequireQualifiedAccess>]
module Matrix =
    let tryMake values =
        if Array2D.length1 values > 0 && Array2D.length2 values > 0 then
            { Matrix.Values = Array2D.copy values }
            |> Success
        else
            sprintf "Matrices must have positive dimensions (given: %ix%i)" (Array2D.length1 values) (Array2D.length2 values)
            |> Failure
    let identity size =
        Array2D.init size size (fun i j -> if i = j then 1 else 0)
        |> tryMake
    let values matrix =
        matrix.Values
        |> Array2D.copy
    let map f matrix =
        { Matrix.Values = matrix.Values |> Array2D.map f }
    let mapi f matrix =
        { Matrix.Values = matrix.Values |> Array2D.mapi f }
    let rows (matrix:Matrix<_>) = matrix.Rows
    let cols (matrix:Matrix<_>) = matrix.Cols
    let resize newRows newCols matrix =
        if newRows <= rows matrix && newCols <= cols matrix then
            matrix
            |> values
            |> fun x -> x.[0 .. newRows - 1, 0 .. newCols - 1]
            |> tryMake
        else
            sprintf "Cannot resize a %ix%i matrix to %ix%i; resize can only go smaller" (rows matrix) (cols matrix) newRows newCols
            |> Failure
    let inline multiply b a =
        if cols a = rows b then
            Array2D.init (rows a) (cols b)
                (fun i j ->
                    Array.map2
                        (*)
                        (a.Row i)
                        (b.Col j)
                    |> Array.sum)
            |> tryMake
        else
            sprintf "Cannot multiply a %ix%i matrix by a %ix%i matrix" (rows a) (cols a) (rows b) (cols b)
            |> Failure
    let inline private removeCross row col matrix =
        Array2D.init
            (rows matrix - 1)
            (cols matrix - 1)
            (fun i j ->
                let i = if i < row then i else i + 1
                let j = if j < col then j else j + 1
                matrix.[i, j])
        |> tryMake
    let inline determinant matrix =
        let rec determinant matrix =
            if rows matrix = cols matrix then
                if rows matrix = 1 then matrix.[0, 0] |> Success
                elif rows matrix > 4 then Failure "Determinant is not implemented for matrices of size > 4"
                else
                    matrix.Row 0
                    |> Array.mapi
                        (fun i value ->
                            removeCross 0 i matrix
                            |> Attempt.bind determinant
                            |> Attempt.map ((*) (if i % 2 = 0 then value else -value)))
                    |> fun values ->
                        match Array.tryPick (function | Failure f -> Some f | Success _ -> None ) values with
                        | Some fail -> Failure fail
                        | None ->
                            values
                            |> Array.choose
                                (function
                                    | Success x -> Some x
                                    | Failure _ -> None)
                            |> Array.sum
                            |> Success
            else
                sprintf "Non-square %ix%i matrix does not have a determinant" (rows matrix) (cols matrix)
                |> Failure
        determinant matrix
    let transpose matrix =
        { Matrix.Values =
            Array2D.init
                (cols matrix)
                (rows matrix)
                (fun i j -> matrix.[j, i]) }
    let inline add b a =
        if rows a = rows b && cols a = cols b then
            Array2D.init (rows a) (cols a)
                (fun i j -> a.[i, j] + b.[i, j])
            |> tryMake
        else
            sprintf "Cannot add a %ix%i matrix by a %ix%i matrix" (rows a) (cols a) (rows b) (cols b)
            |> Failure
    let inline scale scalar =
        map ((*) scalar)
    let inline cofactors matrix =
        if rows matrix = cols matrix then
            if rows matrix = 1 then Array2D.create 1 1 (matrix.[0, 0] / matrix.[0, 0]) |> tryMake
            else
                Array2D.init
                    (rows matrix)
                    (cols matrix)
                    (fun i j ->
                        matrix
                        |> removeCross i j
                        |> Attempt.bind determinant
                        |> Attempt.map (fun x -> if (i + j) % 2 = 0 then x else -x))
                |> fun values ->
                    match values |> Array2D.flatten |> Seq.tryPick (function | Failure f -> Some f | Success _ -> None ) with
                    | Some fail -> Failure fail
                    | None ->
                        values
                        |> Array2D.map Attempt.getSuccess
                        |> tryMake
        else
            sprintf "Non-square %ix%i matrix does not have a cofactor matrix" (rows matrix) (cols matrix)
            |> Failure
    let inline adjugate matrix =
        matrix
        |> cofactors
        |> Attempt.map transpose
    let inverse (matrix:Matrix<float>) =
        determinant matrix
        |> Attempt.bind
            (fun det ->
                if neq0 det then
                    matrix
                    |> adjugate
                    |> Attempt.map (scale (1.0 / det))
                else
                    Failure "Cannot find inverse matrix, determinant is 0")
