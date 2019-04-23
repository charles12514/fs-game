open Engine
open Game
open Game.Things
open System
open Util.Geometry

let populate1 scene (state:State) =
    let parent = scene :> Thing |> Some

    let player0 = Player Input.KeyBinding.Player0
    Thing.setParent parent player0

    let camera = Thing ()
    camera.Actions <- Actions.quit :: camera.Actions
    state.Cameras <- camera :: state.Cameras
    Thing.setParent (player0 :> Thing |> Some) camera

    let player1 = Player Input.KeyBinding.Player1
    player1.RelativePosition <- Vec -50.0 0.0
    Thing.setParent parent player1

    let rock = Rock ()
    rock.RelativePosition <- Vec 100.0 100.0
    Thing.setParent parent rock
    
    let ghostRock = GhostRock ()
    ghostRock.RelativePosition <- Vec 150.0 100.0
    Thing.setParent parent ghostRock

    let skyRock = SkyRock ()
    skyRock.RelativePosition <- Vec 200.0 100.0
    Thing.setParent parent skyRock

    let backgroundRock = BackgroundRock ()
    backgroundRock.RelativePosition <- Vec 250.0 100.0
    Thing.setParent parent backgroundRock

    let fence = FenceH ()
    fence.RelativePosition <- Vec -70.0 -40.0
    Thing.setParent parent fence
    
    let fence = FenceH ()
    fence.RelativePosition <- Vec -130.0 -40.0
    Thing.setParent parent fence

let populate2 scene =
    let parent = scene :> Thing |> Some

    let rock = Rock ()
    rock.RelativePosition <- Vec 100.0 100.0
    Thing.setParent parent rock

let makeTeleporter from dest position =
    let teleporter = Teleporter dest
    teleporter.RelativePosition <- position
    Thing.setParent (Some from) teleporter

[<EntryPoint>]
[<STAThread>]
let main _ =
    let state = State ()
    let scene1 = Scene ()
    Thing.setParent (state :> Thing |> Some) scene1
    let scene2 = Scene ()
    Thing.setParent (state :> Thing |> Some) scene2

    populate1 scene1 state
    populate2 scene2

    makeTeleporter scene1 scene2 <| Vec 50.0 -50.0
    makeTeleporter scene2 scene1 <| Vec 100.0 -50.0

    Game.run "Haldo!" state state.Cameras.Head
    0
