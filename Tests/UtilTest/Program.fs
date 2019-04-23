open FsCheck
open System
open Tests
open Util

[<EntryPoint>]
let main _ =
    Arb.register<ArrayGenerators> () |> ignore
    Arb.register<MatrixGenerators> () |> ignore
    Check.QuickAll<MatrixTests> ()

    printfn "Done!"
    Console.ReadLine () |> ignore

    0
