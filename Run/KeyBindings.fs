namespace Game.Input

open System.Windows.Input

type PlayerControl =
  { Up : Key
    Down : Key
    Left : Key
    Right : Key }

[<RequireQualifiedAccess>]
module KeyBinding =
    let Quit = Key.Escape
    let Pause = Key.P
    let Unpause = Key.O

    let Player0 =
      { Up = Key.W
        Down = Key.S
        Left = Key.A
        Right = Key.D }

    let Player1 =
      { Up = Key.Up
        Down = Key.Down
        Left = Key.Left
        Right = Key.Right }
