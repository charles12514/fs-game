// http://www.dma.fi.upm.es/mabellanas/tfcs/skeleton/html/documentacion/Straight%20Skeletons%20Implementation.pdf
// http://www.mpi-inf.mpg.de/departments/d1/teaching/ss10/Seminar_CGGC/Slides/09_Dinu_SSke.pdf

[<RequireQualifiedAccess>]
module Util.StraightSkeleton

open Geometry
open Util

module private Helpers =

    // Add a segment to the straight skeleton
    // Eliminates 1-point segments and duplicates
    let private tryAdd seg skeletonSegments =
        if (Segment.mag2 seg |> gt0) && (skeletonSegments |> List.exists (Segment.isEquivalent seg) |> not) then
            seg :: skeletonSegments
        else skeletonSegments

    type Vertex (point) =
        member this.point : Vector = point
        member val isProcessed = false with get, set
        member val vertexA : Vertex option = None with get, set
        member val vertexB : Vertex option = None with get, set
        member val edgeA : Segment option = None with get, set
        member val edgeB : Segment option = None with get, set
        member this.bisector () : Ray =
            match (this.edgeA, this.edgeB) with
            | (None, _) | (_, None) -> invalidOp "Cannot get bisector: one or both edges are not defined"
            | (Some a, Some b) -> Vector.bisector -a.Dir b.Dir |> Ray.init point

    [<NoComparison>]
    type StraightSkeleton =
      { Segments : Segment list
        ActiveVertices : Vertex list }
        
    let private tieBreaker = ref 0

    // Event Interface
    // Implements comparable by distance to edges, break ties by event type
    [<AbstractClass>]
    type Event (distSq, priority) =
        do
            tieBreaker := !tieBreaker + 1
        member this.distSq = distSq
        member this.priority = priority
        member this.tieBreaker = !tieBreaker
        interface System.IComparable<Event> with
            member this.CompareTo other =
                if ltE this.distSq other.distSq then -1
                elif gtE this.distSq other.distSq then 1
                else
                    let priorityDiff = this.priority - other.priority
                    if priorityDiff <> 0 then priorityDiff
                    else this.tieBreaker - other.tieBreaker
        interface System.IComparable with
            member this.CompareTo other =
                match other with
                | :? Event as other -> (this :> System.IComparable<Event>).CompareTo other
                | _ -> failwith "Cannot compare event with non-event"
        override this.Equals other = (this :> System.IComparable).CompareTo other = 0
        override this.GetHashCode () = hash this.distSq ^^^ hash this.priority
        abstract member resolve : StraightSkeleton -> StraightSkeleton * Event list
    
    // Edge Event
    // A single edge collapses to a point (bisectors to either side intersect)
    type EdgeEvent =
        inherit Event
        val collapsePoint : Vector // Point of collapse
        val vertexA : Vertex // Vertices to both sides of the edge
        val vertexB : Vertex
        private new (collapsePoint, vertexA, vertexB, distSq) =
            {
                inherit Event (distSq, 0)
                collapsePoint = collapsePoint
                vertexA = vertexA
                vertexB = vertexB
            }

        override this.ToString () =
            sprintf "edge(I=%A, Va=%A, Vb=%A, dist2=%f)" this.collapsePoint this.vertexA.point this.vertexB.point this.distSq

        // Find all edge events with start
        // Look to previous and next vertex, check bisectors for intersection
        static member create (start:Vertex) =
            let startBisector = start.bisector ()
            let tryGetCollapseEvent (vertex:Vertex) edge =
                let vertexBisector = vertex.bisector ()
                let collapsePoint =
                    match Ray.rayIntersection startBisector vertexBisector with
                    | Point pt -> Some pt
                    | NoIntersect -> None
                    | Infinite ->
                        // TODO not sure if this is right
                        // Overlapping rays, only valid case is
                        // O---O------------>>
                        // use ^ as collapse point
                        if eqE <| Vector.sqDistanceTo start.point vertexBisector.RayStart <| Ray.sqDistanceToPoint vertexBisector.RayStart startBisector then Some start.point
                        else None
                collapsePoint
                |> Option.bind (fun pt -> EdgeEvent (pt, start, vertex, Line.sqDistanceToPoint pt (Segment.toLine edge)) |> Some)
            [ tryGetCollapseEvent start.vertexA.Value start.edgeA.Value
              tryGetCollapseEvent start.vertexB.Value start.edgeB.Value ]
            |> List.choose (Option.bind (fun edgeEvent -> edgeEvent :> Event |> Some))

        // Perform the edge collapse
        // Add to straight skeleton and relink adjacent edges to new collapse vertex
        override this.resolve skeleton =
            // The edge has already collapsed
            if this.vertexA.isProcessed && this.vertexB.isProcessed then
                (skeleton, [])
            // Our vertices still need to be adjacent
            elif this.vertexA.vertexB.Value <> this.vertexB || this.vertexB.vertexA.Value <> this.vertexA then
                // TODO this isn't in the paper but it seems to make it work correctly
                (skeleton, [])
            // Only 3 points left in LAV, collapse all to 1 point
            elif this.vertexA.vertexA = this.vertexB.vertexB then
                let vertexC = this.vertexA.vertexA.Value
                this.vertexA.isProcessed <- true
                this.vertexB.isProcessed <- true
                vertexC.isProcessed <- true

                ({ Segments = // Add triangle segments
                    skeleton.Segments
                    |> tryAdd (Seg this.vertexA.point this.collapsePoint)
                    |> tryAdd (Seg this.vertexB.point this.collapsePoint)
                    |> tryAdd (Seg vertexC.point this.collapsePoint)
                   // Remove empty LAV
                   ActiveVertices = List.filter (fun v -> v <> this.vertexA && v <> this.vertexB && v <> vertexC) skeleton.ActiveVertices },
                 [])
            else
                this.vertexA.isProcessed <- true
                this.vertexB.isProcessed <- true

                // Create collapse point
                let collapseVertex = Vertex this.collapsePoint

                // Hook V up to new neighbors
                collapseVertex.vertexA <- this.vertexA.vertexA
                collapseVertex.vertexA.Value.vertexB <- Some collapseVertex
                collapseVertex.vertexB <- this.vertexB.vertexB
                collapseVertex.vertexB.Value.vertexA <- Some collapseVertex

                collapseVertex.edgeA <- this.vertexA.edgeA
                collapseVertex.edgeB <- this.vertexB.edgeB

                ({ Segments =
                    // Add the collapse to the skeleton
                    skeleton.Segments
                    |> tryAdd (Seg this.vertexA.point this.collapsePoint)
                    |> tryAdd (Seg this.vertexB.point this.collapsePoint)
                   ActiveVertices =
                    // Va and Vb are no longer active, replace with V in all LAVs
                    List.map (fun v -> if v = this.vertexA || v = this.vertexB then collapseVertex else v) skeleton.ActiveVertices },
                 // Check for new edge events between V and neighbors
                 EdgeEvent.create collapseVertex)

    let flattenLAV (v:Vertex) =
        let rec flatten x =
            seq {
                yield x
                if x <> v then
                    yield! flatten x.vertexB.Value }
        flatten v.vertexB.Value
    
    // Test if the given edge is present in the LAV containing v
    let private edgeInLav edge start =
        flattenLAV start
        |> Seq.exists (fun vertex -> vertex.edgeB.Value = edge)

    // Merge Event
    // 2 edges collapse to a single line segment
    // Since all our edges are axis-aligned we don't have real split events, this is our equivalent
    type MergeEvent =
        inherit Event
        val private edgeA : Segment // Edges to merge
        val private edgeB : Segment
        private new (edgeA, edgeB) =
            {
                inherit Event (Line.sqDistanceToLine (Segment.toLine edgeA) (Segment.toLine edgeB) / 4.0, 1)
                edgeA = edgeA
                edgeB = edgeB
            }

        override this.ToString () =
            sprintf "merge(ea=%A, eb=%A, dist2=%f)" this.edgeA this.edgeB this.distSq

        // Find all merge events with the segment starting at x (x.eb)
        static member create (x:Vertex) =
            let startEdge = x.edgeB.Value
            let line = Segment.toLine startEdge

            let tryCreateMerge (v:Vertex) =
                let thisEdge = v.edgeB.Value
                if startEdge <> thisEdge // Keep duplicate merges out of the queue
                   && Line.lineIntersection line (Segment.toLine thisEdge) = NoIntersect // Edges must be parallel (lines can't intersect or be coincident)
                   // Edges must be facing each other (each edge is on the other's left)
                   && startEdge.Dir |> Vector.cross (thisEdge.Start - startEdge.Start) |> gt0
                   && thisEdge.Dir |> Vector.cross (startEdge.Start - thisEdge.Start) |> gt0 then
                    let mergeLine = Line.init (startEdge.Start + thisEdge.Start |> (*) 0.5) startEdge.Dir
                    let forcePoint = function | Point p -> p | Infinite | NoIntersect -> invalidArg "intersection" "intersection must be a single point"
                    let startBisectorA = Line.rayIntersection (x.bisector ()) mergeLine |> forcePoint
                    let startBisectorB = Line.rayIntersection (x.vertexB.Value.bisector ()) mergeLine |> forcePoint
                    let thisBisectorA = Line.rayIntersection (v.bisector ()) mergeLine |> forcePoint
                    let thisBisectorB = Line.rayIntersection (v.vertexB.Value.bisector ()) mergeLine |> forcePoint

                    let startALen = Line.getL startBisectorA mergeLine
                    let startBLen = Line.getL startBisectorB mergeLine
                    let thisALen = Line.getL thisBisectorA mergeLine
                    let thisBLen = Line.getL thisBisectorB mergeLine

                    // If this is false one of the edges will collapse before the merge
                    if ltE startALen startBLen && ltE thisBLen thisALen
                       // The edges need to actually merge (the wavefronts don't miss each other)
                       && not (ltE startBLen thisBLen || gtE startALen thisALen) then
                        MergeEvent (startEdge, thisEdge) :> Event |> Some
                    else None
                else None

            flattenLAV x
            |> Seq.toList
            |> List.choose tryCreateMerge
        
        // Perform the merge
        // Add to straight skeleton, split LAV and relink to new vertices
        override this.resolve skeleton =
            // Find the LAV containing both edges
            List.tryFind (fun lav -> edgeInLav this.edgeA lav && edgeInLav this.edgeB lav) skeleton.ActiveVertices
            // Edges aren't in the same LAV any more (or one/both have been collapsed), no collision
            |> Option.bind (fun v ->
                // Find current endpoints of our opposite edges
                let findEdge e =
                    let start = v |> flattenLAV |> Seq.find (fun x -> x.edgeB.Value = e)
                    (start, start.vertexB.Value)
                let (startA, endA) = findEdge this.edgeA
                // ea has already been processed
                if startA.isProcessed && endA.isProcessed then None
                else
                    let (startB, endB) = findEdge this.edgeB
                    // eb has already been processed
                    if startB.isProcessed && endB.isProcessed then None
                    else
                        let mergeLine = Line.init (this.edgeA.Start + this.edgeB.Start |> (*) 0.5) this.edgeA.Dir
                        let getBisectorIntersect (vertex:Vertex) =
                            if vertex.isProcessed then None
                            else
                                Line.rayIntersection (vertex.bisector ()) mergeLine
                                |> function
                                    | Point p -> Some p
                                    | Infinite | NoIntersect -> None
                        let getLastIntersection v1 v2 =
                            match (getBisectorIntersect v1, getBisectorIntersect v2) with
                            | (None, None) -> None
                            | (None, p) | (p, None) -> p
                            | (Some pA, Some pB) ->
                                if Line.getL pA mergeLine < Line.getL pB mergeLine then Some pB
                                else Some pA
                        // Find start point for merged segment
                        getLastIntersection startA endB
                        |> Option.bind (fun start ->
                            // Find end point for merged segment
                            getLastIntersection endA startB
                            |> Option.bind (fun endPt ->
                                // If start comes "after" end, the edges don't merge
                                let startL = Line.getL start mergeLine
                                let endL = Line.getL endPt mergeLine
                                if gtE startL endL then None
                                else
                                    // Endpoints of segment become new vertices
                                    let v1 = Vertex start
                                    let v2 = Vertex endPt

                                    // Hook up V1
                                    v1.vertexA <- Some startA
                                    v1.vertexB <- Some endB
                                    v1.edgeA <- Some this.edgeA
                                    v1.edgeB <- Some this.edgeB
                                    startA.vertexB <- Some v1
                                    endB.vertexA <- Some v1

                                    // Hook up V2
                                    v2.vertexA <- Some startB
                                    v2.vertexB <- Some endA
                                    v2.edgeA <- Some this.edgeB
                                    v2.edgeB <- Some this.edgeA
                                    startB.vertexB <- Some v2
                                    endA.vertexA <- Some v2

                                    ({ Segments = Seg start endPt :: skeleton.Segments // Add merge segment to skeleton
                                       ActiveVertices = v1 :: v2 :: List.filter ((<>) v) skeleton.ActiveVertices }, // Split the LAV
                                     // Check for new edge events caused by added vertices
                                     EdgeEvent.create v1 @ EdgeEvent.create v2)
                                    |> Some)))
            |> function
                | None -> (skeleton, [])
                | Some success -> success

    // Perform one step of the straight skeleton algorithm
    // Attempts to process a single event
    let rec step (skeleton, (eventQueue:Treap<Event>)) =
        match Treap.min eventQueue with
        | None -> (skeleton, eventQueue)
        | Some currentEvent ->
            let eventQueue = Treap.delete currentEvent eventQueue

            // Resolve simultaneous edge events
            // If 2 edge events occur that share a bisector, we need to make sure we're not running over one to do another
            // so we do the one closest to the vertex first since it won't necessarily negate the later one
            let (currentEvent, eventQueue) =
                match currentEvent with
                | :? EdgeEvent as edgeEvent ->
                    match Treap.min eventQueue with
                    | None -> (currentEvent, eventQueue)
                    | Some nextMin ->
                        if eqE currentEvent.distSq nextMin.distSq && currentEvent.priority = nextMin.priority then
                            // Pop all edge events that occur now
                            let rec pullMin (queue:Treap<Event>) events =
                                match Treap.min queue with
                                | None -> (queue, events)
                                | Some min ->
                                    match min with
                                    | :? EdgeEvent as edgeEventMin ->
                                        if eqE currentEvent.distSq min.distSq && currentEvent.priority = min.priority then
                                            pullMin (Treap.delete min queue) (edgeEventMin :: events)
                                        else (queue, events)
                                    | _ -> (queue, events)
                            let eventQueue, edgeEventsNow = pullMin eventQueue [ edgeEvent ]
                            // Map existing vertices to the events that will occur closest to them
                            let firstEventsFrom = System.Collections.Generic.Dictionary<Vertex, EdgeEvent list> ()
                            let tryInsertEdgeEvent current v =
                                // No events already checked for this vertex
                                if firstEventsFrom.ContainsKey v |> not then
                                    firstEventsFrom.[v] <- [ current ]
                                // Resolve conflicts
                                else
                                    let closestEventDist = Line.getL <| (List.head firstEventsFrom.[v]).collapsePoint <| Ray.toLine (v.bisector ())
                                    let thisEventDist = Line.getL current.collapsePoint <| Ray.toLine (v.bisector ())

                                    // Event occurs at the same distance from v as current best, add to list
                                    if eqE closestEventDist thisEventDist then
                                        firstEventsFrom.[v] <- current :: firstEventsFrom.[v]
                                    // Event occurs closer to v than current best, preempt best
                                    elif ltE thisEventDist closestEventDist then
                                        firstEventsFrom.[v] <- [ current ]
                            for current in edgeEventsNow do
                                tryInsertEdgeEvent current current.vertexA
                                tryInsertEdgeEvent current current.vertexB
                            // Find an event that isn't preempted (is one of the closest events to both its vertices)
                            let currentEvent =
                                edgeEventsNow
                                |> List.find
                                    (fun thisEvent ->
                                        firstEventsFrom.[thisEvent.vertexA] |> List.exists (fun e -> e.tieBreaker = thisEvent.tieBreaker)
                                        && firstEventsFrom.[thisEvent.vertexB] |> List.exists (fun e -> e.tieBreaker = thisEvent.tieBreaker))
                                :> Event
                            // Requeue the other events
                            let eventQueue =
                                edgeEventsNow
                                |> List.fold
                                    (fun queue event ->
                                        if event.tieBreaker = currentEvent.tieBreaker then queue
                                        else Treap.insert (event :> Event) queue)
                                    eventQueue
                            (currentEvent, eventQueue)
                        else (currentEvent, eventQueue)
                | _ -> (currentEvent, eventQueue)

            let (skeleton, newEvents) = currentEvent.resolve skeleton
            (skeleton, List.fold (fun queue event -> Treap.insert event queue) eventQueue newEvents)

    module Validation =
        // TODO this and next fun are duplicates of somewhere
        let rec private intersectsAnyAtMid this =
            function
            | [] -> false
            | seg :: segments ->
                match Segment.segmentIntersection this seg with
                | NoIntersect -> intersectsAnyAtMid this segments
                | Infinite -> true
                | Point x ->
                    if (x = this.Start || x = this.End) && (x = seg.Start || x = seg.End) then intersectsAnyAtMid this segments
                    else true
        // Test if the skeleton is not self-intersecting
        // Returns true if the only intersects are at endpoints
        //		false otherwise
        let rec isNonIntersecting =
            function
            | [] -> true
            | this :: segments ->
                if intersectsAnyAtMid this segments then false
                else isNonIntersecting segments
        // Test if the skeleton is a tree (doesn't form a loop)
        // Returns true if it is a tree
        //		false otherwise
        let isTree skeleton =
            // Build graph of skeleton
            let g = skeleton |> Geometry.toUnweightedSegmentGraph |> Graph.asDag
            let vis = initDictionary (Graph.vertices g) false

            // DFS through g (DAG) to test for cycles
            // Returns true if the undirected g was a tree (no cycles)
            //		false if the undirected g contained a cycle
            let rec dfs x =
                if vis.[x] then false // Revisiting a node means there are 2 paths to get here -> undirected g has a cycle
                else
                    vis.[x] <- true
                    g.[x] |> Seq.forall (fun edge -> dfs edge.endVertex)
                    // If a single outward path discovers a cycle, report it
                    // No outward paths contain a cycle, so the subgraph rooted at x is a tree

            if g |> Graph.vertices |> Seq.head |> dfs |> not then // If we find a cycle in 0's subgraph, the skeleton is not a tree
                false
            else
                // If there are unvisited nodes the graph is disconnected (not a tree)
                vis |> Seq.forall (fun kv -> kv.Value)
        
    // Perform validation
    // Returns true if the straight skeleton is not self-intersecting and is a tree
    //  false otherwise
    let isValid skeleton = Validation.isNonIntersecting skeleton && Validation.isTree skeleton
open Helpers

let straightSkeleton polygon =
    // TODO no self-intersection
    // Input validation
    // Remove duplicate points (segments with 0 length)
    let rec reduce p =
        let p2 = p |> Polygon.choose2 (fun (a, b) -> if a = b then None else Some a)
        if Polygon.count p = Polygon.count p2 then p
        else reduce p2
    let polygon = reduce polygon

    // Create LAV from input polygon
    let first = Vertex polygon.[0]
    let segs = Polygon.segments polygon
    first.vertexA <- Some first
    first.vertexB <- Some first
    first.edgeA <- Some segs.[segs.Length - 1]
    first.edgeB <- Some segs.[0]

    polygon
    |> Polygon.segments
    |> List.tail
    |> List.iter
        (fun seg ->
            let w = Vertex seg.Start
            let prev = first.vertexA.Value
            w.vertexA <- Some prev
            w.vertexB <- Some first
            first.vertexA <- Some w
            prev.vertexB <- Some w
            w.edgeA <- prev.edgeB
            w.edgeB <- Some seg)

    // Get the initial events
    let queue =
        first
        |> flattenLAV
        |> Seq.fold
            (fun queue v ->
                EdgeEvent.create v @ MergeEvent.create v
                |> List.fold (fun q event -> Treap.insert event q) queue)
            Treap.empty

    // Process everything in the event queue
    let rec processQueue state =
        let (skeleton, queue) = step state
        if queue = Treap.empty then skeleton
        else processQueue (skeleton, queue)

    let skeleton = (processQueue ({ Segments = []; ActiveVertices = [ first ] }, queue)).Segments

    if isValid skeleton then Some skeleton
    else None
