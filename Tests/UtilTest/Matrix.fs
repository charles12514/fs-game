namespace Tests

open FsCheck
open Util

[<AutoOpen>]
module private Help =
    [<Literal>]
    let maxSize = 20
    [<Literal>]
    let maxSizeForDet = 3
    let floatMax = 100.0
    let getBoolAttempt =
        function
        | Success b -> b
        | Failure _ -> false
    let matrixEqual a b =
        if Matrix.rows a = Matrix.rows b && Matrix.cols a = Matrix.cols b then
            seq {
                for i in 0 .. Matrix.rows a - 1 do
                    for j in 0 .. Matrix.cols a - 1 do
                        yield a.[i, j] = b.[i, j]
            }
            |> Seq.forall id
        else false

type FloatValue = FloatValue of float with
    static member op_Explicit (FloatValue x) = x
type NonZero2DArray<'a> = NonZero2DArray of 'a[,]
type Square2DArray<'a> = Square2DArray of 'a[,]

type ArrayGenerators =
    static member FloatValue () =
        Arb.from<float>
        |> Arb.filter (fun x -> (System.Double.IsInfinity x || System.Double.IsNaN x) |> not)
        |> Arb.convert
            (fun x -> (x % floatMax) |> FloatValue)
            float
    static member NonZero2DArray<'a> () =
        Arb.from<'a[,]>
        |> Arb.filter (fun x -> Array2D.length1 x > 0 && Array2D.length2 x > 0)
        |> Arb.convert
            NonZero2DArray
            (fun (NonZero2DArray x) -> x)
    static member Square2DArray<'a> () =
        Arb.from<NonZero2DArray<'a>>
        |> Arb.convert
            (fun (NonZero2DArray x) ->
                let len = min (Array2D.length1 x) (Array2D.length2 x)
                Array2D.init
                    len
                    len
                    (fun i j -> x.[i, j])
                |> Square2DArray)
            (fun (Square2DArray x) -> x |> NonZero2DArray)

type MatrixSize = MatrixSize of int
type MatrixSizeDet = MatrixSizeDet of int
type MatrixA<'a> = MatrixA of Matrix<'a>
type SameSizeMatrices<'a> = SameSizeMatrices of Matrix<'a> * Matrix<'a>
type MultipliableMatrices<'a> = MultipliableMatrices of Matrix<'a> * Matrix<'a>
type SquareMatrix<'a> = SquareMatrix of Matrix<'a>
type SquareMatrixDet = SquareMatrixDet of Matrix<float>
type SameSizeMatricesDet = SameSizeMatricesDet of Matrix<float> * Matrix<float>

type MatrixGenerators =
    static member MatrixSize () = Gen.choose (1, maxSize) |> Gen.map MatrixSize |> Arb.fromGen
    static member MatrixSizeDet () = Gen.choose (1, maxSizeForDet) |> Gen.map MatrixSizeDet |> Arb.fromGen
    static member MatrixA<'a> () =
        Arb.from<NonZero2DArray<'a>>
        |> Arb.convert
            (fun (NonZero2DArray a) ->
                a
                |> Matrix.tryMake
                |> Attempt.getSuccess
                |> MatrixA)
            (fun (MatrixA m) ->
                m
                |> Matrix.values
                |> NonZero2DArray)
    static member SameSizeMatrices<'a> () =
        Arb.from<MatrixA<'a> * MatrixA<'a>>
        |> Arb.convert
            (fun (MatrixA a, MatrixA b) ->
                let rows = min <| Matrix.rows a <| Matrix.rows b
                let cols = min <| Matrix.cols a <| Matrix.cols b
                let resize =
                    Matrix.resize rows cols
                    >> Attempt.getSuccess
                SameSizeMatrices (resize a, resize b))
            (fun (SameSizeMatrices (a, b)) ->
                (MatrixA a, MatrixA b))
    static member MultipliableMatrices<'a> () =
        Arb.from<MatrixA<'a> * MatrixA<'a>>
        |> Arb.convert
            (fun (MatrixA a, MatrixA b) ->
                let k = min <| Matrix.cols a <| Matrix.rows b
                let a =
                    a
                    |> Matrix.resize (Matrix.rows a) k
                    |> Attempt.getSuccess
                let b =
                    b
                    |> Matrix.resize k (Matrix.cols b)
                    |> Attempt.getSuccess
                MultipliableMatrices (a, b))
            (fun (MultipliableMatrices (a, b)) ->
                (MatrixA a, MatrixA b))
    static member SquareMatrix<'a> () =
        Arb.from<Square2DArray<'a>>
        |> Arb.convert
            (fun (Square2DArray x) ->
                x
                |> Matrix.tryMake
                |> Attempt.getSuccess
                |> SquareMatrix)
            (fun (SquareMatrix m) ->
                m
                |> Matrix.values
                |> Square2DArray)
    static member SquareMatrixDet () =
        Arb.from<SquareMatrix<FloatValue>>
        |> Arb.convert
            (fun (SquareMatrix m) ->
                let len = min maxSizeForDet <| Matrix.rows m
                m
                |> Matrix.map (fun (FloatValue x) -> x)
                |> Matrix.resize len len
                |> Attempt.getSuccess
                |> SquareMatrixDet)
            (fun (SquareMatrixDet m) ->
                m
                |> Matrix.map FloatValue
                |> SquareMatrix)
    static member SameSizeMatricesDet () =
        Arb.from<SquareMatrixDet * SquareMatrixDet>
        |> Arb.convert
            (fun (SquareMatrixDet a, SquareMatrixDet b) ->
                let rows = min <| Matrix.rows a <| Matrix.rows b
                let cols = min <| Matrix.cols a <| Matrix.cols b
                let resize =
                    Matrix.resize rows cols
                    >> Attempt.getSuccess
                SameSizeMatricesDet (resize a, resize b))
            (fun (SameSizeMatricesDet (a, b)) ->
                (SquareMatrixDet a, SquareMatrixDet b))

type MatrixTests =
    // identity properties
    static member `` I * I = I `` (MatrixSize n) =
        Matrix.identity n
        |> Attempt.bind
            (fun i ->
                Matrix.multiply i i
                |> Attempt.map (matrixEqual i))
        |> getBoolAttempt

    // determinant properties
    static member `` det(I) = 1 `` (MatrixSizeDet n) =
        Matrix.identity n
        |> Attempt.bind Matrix.determinant
        |> function
            | Success 1 -> true
            | _ -> false
    static member `` det(2x2) `` a b c d =
        let m = Array2D.zeroCreate 2 2
        m.[0, 0] <- a
        m.[0, 1] <- b
        m.[1, 0] <- c
        m.[1, 1] <- d
        m
        |> Matrix.tryMake
        |> Attempt.bind Matrix.determinant
        |> function
            | Success det -> det = a * d - b * c
            | Failure _ -> false
    static member `` det(cA) = (c^n) det(A) `` (SquareMatrixDet a) (FloatValue c) =
        let left =
            a
            |> Matrix.scale c
            |> Matrix.determinant
            |> Attempt.getSuccess
        let right =
            a 
            |> Matrix.determinant
            |> Attempt.getSuccess
            |> (*) (c ** (Matrix.rows a |> float))
        printfn "left = %f" left
        printfn "right = %f" right
        eqE left right
    static member `` det(AB) = det(A) x det(B) `` (SameSizeMatricesDet (a, b)) =
        let left =
            a
            |> Matrix.multiply b
            |> Attempt.bind (Matrix.determinant)
            |> Attempt.getSuccess
        let right =
            (a |> Matrix.determinant |> Attempt.getSuccess)
            |> (*) (b |> Matrix.determinant |> Attempt.getSuccess)
        left = right

    // transpose properties
    static member `` (A^T)^T = A `` (MatrixA m:MatrixA<int>) =
        m
        |> Matrix.transpose
        |> Matrix.transpose
        |> matrixEqual m
    static member `` (A + B)^T = A^T + B^T `` (SameSizeMatrices (a, b):SameSizeMatrices<int>) =
        let left =
            a
            |> Matrix.add b
            |> Attempt.map Matrix.transpose
            |> Attempt.getSuccess
        let right =
            a
            |> Matrix.transpose
            |> Matrix.add (Matrix.transpose b)
            |> Attempt.getSuccess
        matrixEqual left right
    static member `` (AB)^T = (B^T)(A^T) `` (MultipliableMatrices (a, b):MultipliableMatrices<int>) =
        let left =
            a
            |> Matrix.multiply b
            |> Attempt.map Matrix.transpose
            |> Attempt.getSuccess
        let right =
            b
            |> Matrix.transpose
            |> Matrix.multiply (Matrix.transpose a)
            |> Attempt.getSuccess
        matrixEqual left right
    static member `` (cA)^T = c(A^T) `` (MatrixA a:MatrixA<int>) c =
        let left =
            a
            |> Matrix.scale c
            |> Matrix.transpose
        let right =
            a
            |> Matrix.transpose
            |> Matrix.scale c
        matrixEqual left right
    static member `` det(A^T) = det(A) `` (SquareMatrixDet a) =
        let left =
            a
            |> Matrix.transpose
            |> Matrix.determinant
            |> Attempt.getSuccess
        let right =
            a
            |> Matrix.determinant
            |> Attempt.getSuccess
        left = right

    // inverse properties
    static member `` (A^-1)^-1 = A `` (SquareMatrixDet a) =
        let detA =
            a
            |> Matrix.determinant
            |> Attempt.getSuccess
        if neq0 detA then
            a
            |> Matrix.inverse
            |> Attempt.bind (Matrix.inverse)
            |> Attempt.getSuccess
            |> matrixEqual a
        else true
    static member `` (A^T)^-1 = (A^-1)^T `` (SquareMatrixDet a) =
        let detA =
            a
            |> Matrix.determinant
            |> Attempt.getSuccess
        if neq0 detA then
            let left =
                a
                |> Matrix.transpose
                |> Matrix.inverse
                |> Attempt.getSuccess
            let right =
                a
                |> Matrix.inverse
                |> Attempt.getSuccess
                |> Matrix.transpose
            matrixEqual left right
        else true
    static member `` det(A^-1) = 1 / det(A) `` (SquareMatrixDet a) =
        let detA =
            a
            |> Matrix.determinant
            |> Attempt.getSuccess
        if neq0 detA then
            let detInv =
                a
                |> Matrix.inverse
                |> Attempt.bind (Matrix.determinant)
                |> Attempt.getSuccess
            detInv = 1.0 / detA
        else true
    static member `` (cA)^-1 = (c^-1)(A^-1) `` (SquareMatrixDet a) c =
        let detA =
            a
            |> Matrix.determinant
            |> Attempt.getSuccess
        if neq0 detA && neq0 c then
            let left =
                a
                |> Matrix.scale c
                |> Matrix.inverse
                |> Attempt.getSuccess
            let right =
                a
                |> Matrix.inverse
                |> Attempt.getSuccess
                |> Matrix.scale (1.0 / c)
            matrixEqual left right
        else true
    static member `` (AB)^-1 = (B^-1)(A^-1) `` (SameSizeMatricesDet (a, b)) =
        let detA =
            a
            |> Matrix.determinant
            |> Attempt.getSuccess
        let detB =
            b
            |> Matrix.determinant
            |> Attempt.getSuccess
        if neq0 detA && neq0 detB then
            let left =
                a
                |> Matrix.multiply b
                |> Attempt.getSuccess
                |> Matrix.inverse
                |> Attempt.getSuccess
            let right =
                b
                |> Matrix.inverse
                |> Attempt.getSuccess
                |> Matrix.multiply
                    (a
                     |> Matrix.inverse
                     |> Attempt.getSuccess)
                |> Attempt.getSuccess
            matrixEqual left right
        else true
