namespace Util

type private PairingHeapNode<'v, 'k when 'k : comparison> =
  { Element : 'v
    Key : 'k
    Children : 'v list
    Parent : 'v option }

    override this.ToString () =
        sprintf "(%A, %A) -> %A" this.Element this.Key this.Children

type PairingHeap<'v, 'k when 'v : comparison and 'k : comparison> =
    private
      { Root : 'v option
        ElementLocation : Map<'v, PairingHeapNode<'v, 'k>> }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PairingHeap =

    let empty = { Root = None; ElementLocation = Map.empty }

    let findMin heap =
        match heap.Root with
        | None -> invalidOp "Heap is empty"
        | Some n -> n

    let private merge heap1 heap2 heapOf =
        match (heap1, heap2) with
        | (None, h) | (h, None) -> (h, heapOf)
        | (Some n1, Some n2) ->
            if n1.Key < n2.Key then
                let newRoot = { Element = n1.Element; Key = n1.Key; Children = n2.Element :: n1.Children; Parent = None }
                (Some newRoot,
                    heapOf
                    |> Map.add newRoot.Element newRoot
                    |> Map.add n2.Element { n2 with Parent = Some newRoot.Element })
            else
                let newRoot = { Element = n2.Element; Key = n2.Key; Children = n1.Element :: n2.Children; Parent = None }
                (Some newRoot,
                    heapOf
                    |> Map.add newRoot.Element newRoot
                    |> Map.add n1.Element { n1 with Parent = Some newRoot.Element })

    let insert elem key heap =
        if heap.ElementLocation |> Map.containsKey elem then
            invalidArg "elem" (sprintf "Heap already contains element %A" elem)
        else
            let newHeap = { Element = elem; Key = key; Children = []; Parent = None }
            let newLocs = heap.ElementLocation |> Map.add elem newHeap
            let rootHeap = Option.bind (fun n -> Some newLocs.[n]) heap.Root
            let (newRoot, finalLocs) =
                let (heap, m) = merge (Some newHeap) rootHeap newLocs
                (Option.bind (fun h -> Some h.Element) heap, m)
            { Root = newRoot; ElementLocation = finalLocs }

    let rec private mergePairs heaps heapOf heap =
        match List.length heaps with
        | 0 -> (None, heapOf)
        | 1 -> (Some heaps.[0], heapOf)
        | _ ->
            let (h1, m1) = merge (Some heaps.[0]) (Some heaps.[1]) heapOf
            let (h2, m2) = heap |> mergePairs (heaps |> List.tail |> List.tail) m1
            merge h1 h2 m2
        
    let deleteMin heap =
        match heap.Root with
        | None -> heap
        | Some n ->
            let rootHeap = heap.ElementLocation.[n]
            let (newRoot, finalLocs) =
                match heap |> mergePairs (rootHeap.Children |> List.map (fun c -> heap.ElementLocation.[c])) (heap.ElementLocation |> Map.remove n) with
                | (Some h, m) -> (Some h.Element, m |> Map.add h.Element { m.[h.Element] with Parent = None })
                | (None, m) -> (None, m)
            { Root = newRoot; ElementLocation = finalLocs }

    let decreaseKey elem newKey heap =
        let node = heap.ElementLocation.[elem]
        if node.Key < newKey then heap
        else
            let node = { node with Key = newKey }
            let mapWithKey = heap.ElementLocation |> Map.add elem node
            match node.Parent with
            | Some parElem when newKey <= mapWithKey.[parElem].Key ->
                let par = mapWithKey.[parElem]
                let node = { node with Parent = None }
                let mapWithParent =
                    mapWithKey
                    |> Map.add elem node
                    |> Map.add parElem { par with Children = par.Children |> List.filter (fun n -> n <> elem) }
                let rootHeap =
                    match heap.Root with
                    | Some n -> Some mapWithParent.[n]
                    | None -> None
                let (newRoot, finalLocs) =
                    let (heap, m) = merge rootHeap (Some node) mapWithParent
                    (Option.bind (fun h -> Some h.Element) heap, m)
                { Root = newRoot; ElementLocation = finalLocs }
            | _ -> { Root = heap.Root; ElementLocation = mapWithKey }

    let delete elem heap =
        match heap.Root with
        | Some n -> heap |> decreaseKey elem heap.ElementLocation.[n].Key
        | None -> heap
        |> deleteMin

    let isEmpty heap = heap.Root = None

    let contains elem heap = Map.containsKey elem heap.ElementLocation

    let keyOf elem heap = heap.ElementLocation.[elem].Key

    let minKey heap = keyOf (findMin heap) heap

    let values heap = heap.ElementLocation |> Map.toSeq |> Seq.map fst

    let toSeq heap =
        heap.ElementLocation
        |> Map.toSeq
        |> Seq.map (fun (elem, heap) -> (elem, heap.Key))
