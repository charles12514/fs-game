namespace Util

type internal FibonacciHeapNode<'v, 'k>(value:'v, key:'k) as this =
    member this.value = value
    member val key = key with get, set
    member val numChildren = 0 with get, set
    member val marked = false with get, set
    member val next = Some this with get, set
    member val prev = Some this with get, set
    member val parent : FibonacciHeapNode<'v, 'k> option = None with get, set
    member val children : FibonacciHeapNode<'v, 'k> option = None with get, set

    member this.append n =
        let a = this
        let b = this.next
        a.next <- Some n
        b.Value.prev <- Some n
        n.prev <- Some a
        n.next <- b
    member this.addChild n =
        match this.children with
        | None -> this.children <- Some n
        | Some child -> child.append n
        n.parent <- Some this
        this.numChildren <- this.numChildren + 1
        n.marked <- false
            
type FibonacciHeap<'v, 'k when 'v : equality and 'k : comparison>() =
    member val private forest = None with get, set
    member val private min : FibonacciHeapNode<'v, 'k> option = None with get, set
    member val private values = System.Collections.Generic.Dictionary<'v, FibonacciHeapNode<'v, 'k>>() with get, set

    member this.findMin () =
        match this.min with
        | Some m -> m.value
        | None -> invalidOp "Cannot retrieve min: heap contains no elements"
    member this.minKey () =
        match this.min with
        | Some m -> m.key
        | None -> invalidOp "Cannot retrieve min key: heap contains no elements"
    member this.contains value = this.values.ContainsKey(value)
    member this.isEmpty () = this.min = None
    member this.keyOf value =
        match this.values.TryGetValue value with
        | (true, node) -> node.key
        | (false, _) -> invalidOp (sprintf "Cannot get key of %A: value does not exist in the heap" value)
    member this.getValues () = this.values.Keys |> Seq.map id
    member this.enqueue value key =
        if this.contains value then false
        else
            let n = FibonacciHeapNode (value, key)
            this.values.Add (value, n)
            match this.forest with
            | None -> this.forest <- Some n
            | Some f -> f.append n
            match this.min with
            | None -> this.min <- Some n
            | Some m when key < m.key -> this.min <- Some n
            | _ -> ()
            true
    member this.dequeue () =
        match this.min with
        | None -> invalidOp "Cannot dequeue: heap contains no elements"
        | Some m ->
            let value = m.value
            this.values.Remove value |> ignore
            // Phase 1
            // Children become new roots
            match m.children with
            | None -> ()
            | Some child ->
                let mutable n = child
                for i in 0 .. m.numChildren - 1 do
                    n.marked <- false
                    n.parent <- None
                    n <- n.next.Value
                this.merge this.forest.Value child
                m.children <- None
            // Remove min from forest
            if m.next = Some m then
                this.min <- None
                this.forest <- None
            else
                this.forest <- m.next
                m.prev.Value.next <- m.next
                m.next.Value.prev <- m.prev
            match this.forest with
            | None -> ()
            | Some f ->
                // Phase 2
                // Get list of roots (so we can ignore prev/next pointers in next step)
                let mutable queue = [ f ]
                let mutable n = f.next.Value
                while n <> f do
                    queue <- n :: queue
                    n <- n.next.Value
                // Combine roots with equal degrees
                let root = System.Collections.Generic.Dictionary<int, FibonacciHeapNode<'v, 'k>> ()
                for x in queue do
                    let mutable n = x
                    n.prev <- Some n
                    n.next <- Some n
                    while root.ContainsKey n.numChildren do
                        let mutable a = root.[n.numChildren]
                        let mutable b = n
                        root.Remove n.numChildren |> ignore
                        if b.key < a.key then
                            let t = a
                            a <- b
                            b <- t
                        a.addChild b
                        n <- a
                    root.[n.numChildren] <- n
                // Relink prev/next pointers for finished forest
                this.forest <- None
                for x in root do
                    let n = x.Value
                    n.next <- Some n
                    n.prev <- Some n
                    match this.forest with
                    | None -> this.forest <- Some n
                    | Some f -> f.append n
                // Phase 3
                // Find new min
                this.min <- this.forest
                let mutable n = this.forest.Value.next.Value
                while n <> this.forest.Value do
                    if n.key < this.min.Value.key then
                        this.min <- Some n
                    n <- n.next.Value
            value
    member private this.merge a b =
        let x = a.next.Value
        let y = b.next.Value
        a.next <- Some y
        y.prev <- Some a
        b.next <- Some x
        x.prev <- Some b
    member this.decreaseKey value key =
        match this.values.TryGetValue value with
        | (false, _) -> false
        | (true, n) ->
            let mutable n = n
            // Decrease the key
            if n.key < key then invalidOp "Cannot decrease key: new key is greater"
            n.key <- key
            // Update min
            if key < this.min.Value.key then this.min <- Some n
            if n.parent <> None && key < n.parent.Value.key then
                let mutable finished = false
                while not finished do
                    // Cut this node from its parent and create a new tree
                    finished <- true
                    let par = n.parent.Value
                    n.parent <- None
                    n.marked <- false
                    par.numChildren <- par.numChildren - 1
                    if par.numChildren = 0 then
                        par.children <- None
                    else
                        if par.children = Some n then par.children <- n.next
                        n.next.Value.prev <- n.prev
                        n.prev.Value.next <- n.next
                    n.prev <- Some n
                    n.next <- Some n
                    this.forest.Value.append n
                    // Go up the tree as long as nodes are marked and not the root
                    if par.parent <> None then
                        if par.marked then
                            finished <- false
                            n <- par
                        else par.marked <- true
            true
