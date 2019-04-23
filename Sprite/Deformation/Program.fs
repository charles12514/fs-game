open Deformation
open FsCheck
open System
open System.Numerics
open Util
open Util.Geometry

// conventions:
// x = +right, -left
// y = +up, -down
// z = +out, -in
// (0, 0, 0) is directly between character's feet
// character faces +z

let debug = System.Diagnostics.Debug.WriteLine

let runTests () =
    Arb.register<Tests.Generators> () |> ignore
    Check.QuickAll<Tests.TriangleTransform> ()

[<EntryPoint>]
let main _ =
    Mesh.loadRectangleMesh 12 6 4 (File @"C:\Users\csnyder\Desktop\rect.png")
    |> Mesh.yaw (Math.PI / 4.0)
    |> Mesh.pitch (15.0 * Math.PI / 180.0)
    |> Mesh.toSprite 20 20 10 10
    |> (fun bmp -> bmp.Save(@"C:\Users\csnyder\Desktop\rect_out.bmp"))

//    printfn "Done!"
//    System.Console.ReadLine () |> ignore
    0
