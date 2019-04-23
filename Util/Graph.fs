namespace Util

open Util

type Edge<'v, 'w>(startVertex:'v, endVertex:'v, weight:'w) =
    member this.startVertex = startVertex
    member this.endVertex = endVertex
    member this.weight = weight

[<NoComparison>]
type Graph<'v, 'w> =
    private { g : System.Collections.Generic.Dictionary<'v, Edge<'v, 'w> list> }
    member this.Item vertex =
        match this.g.TryGetValue vertex with
        | true, edges -> edges
        | false, _ -> invalidArg "vertex" <| sprintf "Graph does not contain vertex %A" vertex

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Graph =
    let make vertices edges =
        let g = System.Collections.Generic.Dictionary<'v, Edge<'v, 'w> list> ()
        let addVertex v =
            if g.ContainsKey v then
                invalidArg "v" <| sprintf "Graph already contains vertex %A" v
            g.[v] <- []
        vertices |> Seq.iter addVertex
        let addEdge (e:Edge<'v, 'w>) =
            let containsStart, currentEdges = g.TryGetValue e.startVertex
            if not containsStart then
                invalidArg "e" <| sprintf "Edge vertex %A is not in vertex list" e.startVertex
            if g.ContainsKey e.endVertex |> not then
                invalidArg "e" <| sprintf "Edge vertex %A is not in vertex list" e.endVertex
            g.[e.startVertex] <- e :: currentEdges
        edges |> Seq.iter addEdge
        { g = g }
    let vertices graph = graph.g.Keys |> Seq.readonly
    let count graph = graph.g.Count
    let edges graph = graph.g.Values |> Seq.collect id
    let asDag graph =
        let g = System.Collections.Generic.Dictionary graph.g
        let vertices = vertices graph
        let vis = initDictionary vertices false
        let rec dfs v =
            vis.[v] <- true
            for e in g.[v] do g.[e.endVertex] <- List.filter (fun e1 -> e1.endVertex <> v) g.[e.endVertex]
            for e in g.[v] do dfs e.endVertex
        for v in vertices do
            if not vis.[v] then
                dfs v
        make vertices <| Seq.collect id g.Values

    let topologicalSort g =
        let processed = vertices g |> initDictionary <| false
        let inDegree = vertices g |> initDictionary <| 0
        for e in edges g do
            inDegree.[e.endVertex] <- inDegree.[e.endVertex] + 1
        let q =
            Seq.fold (fun list (kv:System.Collections.Generic.KeyValuePair<'v, int>) ->
                        if kv.Value = 0 then kv.Key :: list
                        else list
                        ) [] inDegree
        let step sorted queue =
            match queue with
            | [] -> sorted, []
            | a :: next ->
                processed.[a] <- true
                let next =
                    g.[a]
                    |> List.fold (fun q e ->
                                    let v = e.endVertex
                                    inDegree.[v] <- inDegree.[v] - 1
                                    if inDegree.[v] = 0 then v :: q
                                    else q
                                    ) next
                a :: sorted, next
        let rec go state =
            match state with
            | _, [] -> state
            | sort, queue -> step sort queue |> go
        let topSort, _ = go ([], q)
        if List.length topSort <> count g then None
        else topSort |> List.rev |> Some

    module MinimumSpanningTree =
        let kruskal (edges:Edge<'v, _> seq) =
            let uf = UnionFind ()
            edges
            |> Seq.sortBy (fun e -> e.weight)
            |> Seq.fold (fun mst e ->
                            match uf.find e.startVertex <> uf.find e.endVertex with
                            | true ->
                                uf.union e.startVertex e.endVertex
                                e :: mst
                            | false -> mst
                        ) []

    module StronglyConnectedComponents =
        let tarjan g =
            let index = vertices g |> initDictionary <| -1
            let lowlink = vertices g |> initDictionary <| 0

            let rec dfs v state =
                let scc, stack, ind = state
                let ind = ind + 1
                lowlink.[v] <- ind
                index.[v] <- ind
                let stack = v :: stack

                let state =
                    List.fold (fun st (w:Edge<'v, _>) ->
                                if index.[w.endVertex] < 0 then
                                    let st = dfs w.endVertex st
                                    lowlink.[v] <- min lowlink.[v] lowlink.[w.endVertex]
                                    st
                                else
                                    let _, thisStack, _ = st
                                    if List.exists (fun i -> w.endVertex = i) thisStack then
                                        lowlink.[v] <- min lowlink.[v] index.[w.endVertex]
                                    st
                              ) (scc, stack, ind) g.[v]

                if lowlink.[v] = index.[v] then
                    let scc, stack, ind = state
                    let rec pop st =
                        match st with
                        | comp, (h :: t) ->
                            let next = h :: comp, t
                            if h = v then next
                            else pop next
                        | _, [] -> invalidOp "Unexpected state in Tarjan's strongly connected components: stack = []"
                    let comp, stack = pop ([], stack)
                    comp :: scc, stack, ind
                else
                    state

            let scc, _, _ =
                vertices g
                |> Seq.fold (fun st i ->
                                if index.[i] < 0 then
                                    dfs i st
                                else
                                    st
                            ) ([], [], -1)
            scc
