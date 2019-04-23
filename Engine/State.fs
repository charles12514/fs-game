namespace Engine

type State () =
    inherit Thing ()

    let keys = System.Collections.Generic.HashSet<System.Windows.Input.Key> ()

    member val Cameras : Thing list = [] with get, set
    member val Quit = false with get, set
    member this.SetKeyDown = keys.Add >> ignore
    member this.SetKeyUp = keys.Remove >> ignore
    member this.IsKeyDown = keys.Contains

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module State =
    let rec private act elapsedTime (state:State) (thing:Thing) =
        thing.Actions
        |> List.iter
            (function
             | SelfUpdate f ->
                f elapsedTime
             | SceneUpdate f ->
                Thing.containingScene thing
                |> Option.iter (fun scene -> f scene elapsedTime)
             | GlobalUpdate f ->
                f state elapsedTime)
        thing.Children
        |> List.iter
            (function
             | :? Scene -> ()
             | child -> act elapsedTime state child)

    let step (elapsedTime:System.TimeSpan) (state:State) =
        if elapsedTime.TotalMilliseconds > 0.0 then
            state.Cameras
            |> List.choose Thing.containingScene
            |> List.distinct
            |> List.iter (act elapsedTime state)
