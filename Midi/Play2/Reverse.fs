module Reverse

open Structure

let debugn = System.Diagnostics.Debug.WriteLine

type Node =
  { Previous : Node option
    Command : Command
    AllowLiteral : bool
    PhraseBuilder : Phrase list
    RemainingNotes : Note list
    Depth : int }

type NodeHeap () =
    let mutable heap =
        [|
            { Previous = None
              Command = Clear
              AllowLiteral = false
              PhraseBuilder = []
              RemainingNotes = []
              Depth = 0 }
        |]
    let mutable index = 0
    let compare a b =
        let lengthA = heap.[a].RemainingNotes |> List.length
        let lengthB = heap.[b].RemainingNotes |> List.length
        if lengthA <> lengthB then
            lengthA - lengthB
        else
            heap.[a].Depth - heap.[b].Depth
    let parentIndex childIndex = childIndex / 2
    let swapWithParent childIndex =
        let parentIndex = parentIndex childIndex
        let temp = heap.[childIndex]
        heap.[childIndex] <- heap.[parentIndex]
        heap.[parentIndex] <- temp
    let rec percolateUp index =
        if index > 1 && compare index (parentIndex index) < 0 then
            swapWithParent index
            index |> parentIndex |> percolateUp
    let rec percolateDown i =
        let child1 = i * 2
        let child2 = i * 2 + 1
        if index < child1 then
            ()
        elif index = child1 && compare child1 i < 0 then
            swapWithParent child1
            percolateDown child1
        else
            let minChild = if compare child1 child2 > 0 then child2 else child1
            if compare minChild i < 0 then
                swapWithParent minChild
                percolateDown minChild

    member this.Length = index
    member this.IsEmpty = index = 0

    member this.Peek () = Array.tryItem 1 heap
    member this.Insert node =
        index <- index + 1
        if index >= heap.Length then
            heap <- Array.append heap heap
        heap.[index] <- node
        percolateUp index
    member this.DeleteMin () =
        if index > 0 then
            heap.[1] <- heap.[index]
            index <- index - 1
            percolateDown 1

/// if we apply commands with current state, will we match the head of remainingNotes?
let matchesHead =
    let rec matchesHead list1 list2 =
        match (list1, list2) with
        | (_, []) -> true
        | (h1 :: t1, h2 :: t2) when h1 = h2 -> matchesHead t1 t2
        | _ -> false
    fun state remainingNotes ->
        List.fold
            (fun (state, outputNotes) cmd ->
                let (state, nextOutput) = Elaborate.applyCommand state cmd
                (state, nextOutput :: outputNotes))
            (state, [])
        >> snd
        >> List.rev
        >> List.choose id
        >> Phrase
        >> Phrase.flatten
        >> Seq.toList
        >> matchesHead remainingNotes

let buildShiftCmds compound state remainingNotes =
    let getFirstNote = Seq.tryFind (function | Note _ -> true | Hold | Rest -> false)
    if compound then
        Compound
        |> Elaborate.applyCommand state
        |> fst
    else state
    |> Elaborate.applyCommand <| Shift 0
    |> snd
    |> Option.map Phrase.flatten
    |> Option.bind getFirstNote
    |> Option.fold
        (fun _ firstNoteNext ->
            match (firstNoteNext, getFirstNote remainingNotes) with
            | (Note a, Some (Note b)) -> int b - int a |> Some
            | _ -> None
            |> function
                | Some s when s <> 0 ->
                    [ [ Shift s ]
                      [ ShiftFirst s ] ]
                    |> List.map
                        (fun shift ->
                            if compound then Compound :: shift
                            else shift)
                | _ -> [])
        []

let buildShiftLastCmd compound state remainingNotes =
    let next =
        if compound then
            Compound
            |> Elaborate.applyCommand state
            |> fst
        else state
        |> Elaborate.applyCommand <| Shift 0
        |> snd
        |> Option.map Phrase.flatten
    next
    |> Option.bind (Seq.tryFindIndexBack (function | Note _ -> true | Hold | Rest -> false))
    |> Option.fold
        (fun _ lastNoteNextIndex ->
            match (Seq.item lastNoteNextIndex next.Value, List.tryItem lastNoteNextIndex remainingNotes) with
            | (Note a, Some (Note b)) -> int b - int a |> Some
            | _ -> None
            |> function
                | Some s when s <> 0 ->
                    [ (if compound then [ Compound; ShiftLast s ] else [ ShiftLast s ]) ]
                | _ -> [])
        []

let buildAllShiftCmds state remainingNotes =
    buildShiftCmds false state remainingNotes
    @ buildShiftCmds true state remainingNotes
    @ buildShiftLastCmd false state remainingNotes
    @ buildShiftLastCmd true state remainingNotes

/// what commands, when applied to state, produce notes at the head of remainingNotes?
let nextCommandOptions allowLiteral state remainingNotes =
    match remainingNotes with
    | [] -> []
    | head :: _ ->
        let literalCmd =
            if allowLiteral then [ [ head |> Single |> Literal ] ]
            else []
        [
            [ Clear; head |> Single |> Literal ]
            [ Compound; Repeat ]
            [ Compound; Reset ]
            [ Repeat ]
            [ Reset ]
        ]
        @ buildAllShiftCmds state remainingNotes
        @ literalCmd
        |> List.filter (matchesHead state remainingNotes)

let rec applyList node commands =
    match commands with
    | command :: commands ->
        let (phraseBuilder, outputNotes) = Elaborate.applyCommand node.PhraseBuilder command
        let noteCount =
            outputNotes
            |> Option.fold (fun _ -> Phrase.flatten >> Seq.length) 0
        let thisNode =
            { Previous = Some node
              Command = command
              AllowLiteral =
                match command with
                | Clear -> true
                | Compound -> false
                | _ -> node.AllowLiteral
              PhraseBuilder = phraseBuilder
              RemainingNotes = List.skip noteCount node.RemainingNotes
              Depth = node.Depth + 1 }
        applyList thisNode commands
    | [] -> node

let nextNodes node =
    nextCommandOptions node.AllowLiteral node.PhraseBuilder node.RemainingNotes
    |> List.map (applyList node)

/// what commands will produce the given notes?
let toCommands notes =
    let heap = NodeHeap ()
    let rec search () =
        if heap.IsEmpty then
            failwith "unreachable"
        elif heap.Peek().Value.RemainingNotes |> List.isEmpty then
            heap.Peek().Value
        else
            let node = heap.Peek().Value
            heap.DeleteMin ()
            nextNodes node
            |> List.iter heap.Insert
            search ()
    heap.Insert
        { Previous = None
          Command = Clear
          AllowLiteral = true
          PhraseBuilder = []
          RemainingNotes = notes
          Depth = 0 }
    let endNode = search ()
    let rec walkBack list =
        function
        | None -> list
        | Some node ->
            walkBack (node.Command :: list) node.Previous
    walkBack [] (Some endNode)
    |> List.tail
