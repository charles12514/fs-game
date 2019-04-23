[<AutoOpen>]
module Common

let split (c:char) (s:string) = s.Split c

type FilePath = FilePath of string
