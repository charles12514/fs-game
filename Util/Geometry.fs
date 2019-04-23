module Util.Geometry

open Constants
open Util

[<CustomEquality>]
[<CustomComparison>]
type Vector =
  { x : float
    y : float }

    static member (+) (a:Vector, b:Vector) = { x = a.x + b.x; y = a.y + b.y }
    static member (*) (a:Vector, m:float) = { x = a.x * m; y = a.y * m }
    static member (*) (m:float, a:Vector) = a * m
    static member (*) (a:Vector, m:int) = a * float m
    static member (*) (m:int, a:Vector) = a * m
    static member (~-) (a:Vector) = a * -1.0
    static member (-) (a:Vector, b:Vector) = a + -b
    static member (/) (a:Vector, m:float) = a * (1.0 / m)
    static member (/) (a:Vector, m:int) = a / float m
    static member op_Equality (a:Vector, b:Vector) = a.Equals b
    static member op_Inequality (a:Vector, b:Vector) = a = b |> not
    static member DivideByInt (a:Vector, b:int) = a / b
    static member Zero = { x = 0.0; y = 0.0 }

    interface System.IComparable<Vector> with
        member a.CompareTo b =
            match (a.x :> System.IComparable<float>).CompareTo b.x with
            | 0 -> (a.y :> System.IComparable<float>).CompareTo b.y
            | n -> n
    interface System.IComparable with
        member a.CompareTo obj =
            match obj with
            | :? Vector as b -> (a :> System.IComparable<Vector>).CompareTo b
            | _ -> invalidArg "obj" "obj is not a Vector"
    interface System.IEquatable<Vector> with
        member a.Equals b = eqE a.x b.x && eqE a.y b.y
    override a.Equals obj =
        match obj with
        | :? Vector as b -> (a :> System.IEquatable<Vector>).Equals b
        | _ -> false
    override a.GetHashCode () = hash a.x ^^^ hash a.y
    override a.ToString () = sprintf "<%f, %f>" a.x a.y

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    let getX v = v.x
    let getY v = v.y
    let dot a b = a.x * b.x + a.y * b.y
    let mag2 v = dot v v
    let mag = mag2 >> sqrt
    let normalize v = v / mag v
    let round v = { x = floorE v.x; y = floorE v.y }
    let rotate angle v =
        let cosA = cos angle
        let sinA = sin angle
        { x = v.x * cosA - v.y * sinA; y = v.x * sinA + v.y * cosA }
    let cross b a = a.x * b.y - a.y * b.x // order
    let project b a = b * (dot a b / mag2 b) // order
    let sqDistanceTo a b = a - b |> mag2
    let distanceTo a b = sqDistanceTo a b |> sqrt
    let angleTo b a = atan2 (a |> cross b) (dot a b) // order
    let bisector b a = // order
        let bisector = a |> rotate (a |> angleTo b |> (*) 0.5) |> normalize
        if a |> cross b |> geq0 then bisector
        else -bisector
    let inline ofLength length v = v |> normalize |> (*) length
    let perpendicular v = { x = -v.y; y = v.x }

type Intersection =
    | NoIntersect
    | Infinite
    | Point of Vector

type Line =
  { LineStart : Vector
    LineDir : Vector }
    override line.ToString () = sprintf "Line(%A, %A)" line.LineStart line.LineDir

type Ray =
  { RayStart : Vector
    RayDir : Vector }
    override ray.ToString () = sprintf "Ray(%A, %A)" ray.RayStart ray.RayDir

type Segment =
  { Start : Vector
    Dir : Vector }
    member this.End = this.Start + this.Dir
    static member op_Equality (a, b) = a.Start = b.Start && a.Dir = b.Dir
    static member op_Inequality (a, b) = a = b |> not
    override segment.ToString () = sprintf "Segment(%A, %A)" segment.Start segment.End

module private Helpers =
    let rayToLine ray = { LineStart = ray.RayStart; LineDir = ray.RayDir }
    let rayClosestPointTo point ray =
        let toPoint = point - ray.RayStart
        if Vector.dot ray.RayDir toPoint <= 0.0 then ray.RayStart
        else Vector.project ray.RayDir toPoint + ray.RayStart
    let raySqDistanceToPoint point ray = point - rayClosestPointTo point ray |> Vector.mag2
    let segmentToLine segment = { LineStart = segment.Start; LineDir = segment.Dir }
    let segmentMag2 segment = Vector.mag2 segment.Dir
    let segmentClosestPointTo point segment =
        if segment.Dir |> Vector.dot (segment.End - point) <= 0.0 then
            segment.End
        else
            let toPoint = point - segment.Start
            if segment.Dir |> Vector.dot toPoint <= 0.0 then
                segment.Start
            else
                (toPoint |> Vector.project segment.Dir) + segment.Start
    let segmentSqDistanceToPoint point segment = point - segmentClosestPointTo point segment |> Vector.mag2
open Helpers

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Line =
    let init start dir = { LineStart = start; LineDir = dir }
    let scalarProjection v line = Vector.dot (v - line.LineStart) line.LineDir / Vector.mag2 line.LineDir
    let getL v line =
        let a = v - line.LineStart
        (Vector.dot a line.LineDir |> sign |> float) * (Vector.mag2 a / Vector.mag2 line.LineDir |> sqrt)
    let shiftLeft (dist:float) line =
        { line with LineStart = line.LineStart + ({ Vector.x = -line.LineDir.y; y = line.LineDir.x } |> Vector.normalize |> (*) dist) }
    let lineIntersection line2 line1 =
        let v = line2.LineStart - line1.LineStart
        let na = v |> Vector.cross line2.LineDir
        let div = line1.LineDir |> Vector.cross line2.LineDir
        if eq0 div then
            if eq0 na && eq0 (v |> Vector.cross line1.LineDir) then Infinite
            else NoIntersect
        else
            Point (line1.LineStart + na / div * line1.LineDir)
    let rayIntersection ray line =
        match ray |> rayToLine |> lineIntersection line with
        | NoIntersect -> NoIntersect
        | Infinite -> Infinite
        | Point p ->
            if ray |> raySqDistanceToPoint p |> leq0 then Point p
            else NoIntersect
    let segmentIntersection segment line =
        match segment |> segmentToLine |> lineIntersection line with
        | NoIntersect -> NoIntersect
        | Infinite -> Infinite
        | Point p ->
            if segment |> segmentSqDistanceToPoint p |> leq0 then Point p
            else NoIntersect
    let closestPointTo point line = (point - line.LineStart |> Vector.project line.LineDir) + line.LineStart
    let sqDistanceToPoint point line = point - closestPointTo point line |> Vector.mag2
    let sqDistanceToLine b a =
        match lineIntersection b a with
        | NoIntersect -> a |> sqDistanceToPoint (b |> closestPointTo a.LineStart)
        | Infinite | Point _ -> 0.0
    let sqDistanceToRay ray line =
        match rayIntersection ray line with
        | NoIntersect -> line |> sqDistanceToPoint ray.RayStart
        | Infinite | Point _ -> 0.0
    let sqDistanceToSegment segment line =
        match segmentIntersection segment line with
        | NoIntersect -> min (sqDistanceToPoint segment.Start line) (sqDistanceToPoint segment.End line)
        | Infinite | Point _ -> 0.0

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Ray =
    let init start dir = { RayStart = start; RayDir = dir }
    let toLine = rayToLine
    let closestPointTo = rayClosestPointTo
    let sqDistanceToPoint = raySqDistanceToPoint
    let lineIntersection line ray = Line.rayIntersection ray line
    let rayIntersection ray2 ray1 =
        let dr = ray1.RayStart - ray2.RayStart
        let d = ray1.RayDir |> Vector.cross ray2.RayDir
        let na = ray2.RayDir |> Vector.cross dr
        let nb = ray1.RayDir |> Vector.cross dr
        if eq0 d then // parallel or coincident lines
            if eq0 na && eq0 nb && (ray1 |> sqDistanceToPoint ray2.RayStart |> eq0 || ray2 |> sqDistanceToPoint ray1.RayStart |> eq0) then
                if ray1.RayStart = ray2.RayStart
                    && ray1.RayDir |> Vector.cross ray2.RayDir |> eq0
                    && ray1.RayDir |> Vector.dot ray2.RayDir |> lt0 then // opposite directions, same start point
                    Point ray1.RayStart
                else // same direction or towards each other
                    Infinite
            else // pointed away from each other
                NoIntersect
        else // skew lines
            let ua = na / d
            let ub = nb / d
            if geq0 ua && geq0 ub then // intersect is forwards along both rays
                Point (ray1.RayStart + ua * ray1.RayDir)
            else // intersect is behind the start on one/both rays
                NoIntersect
    let segmentIntersection segment ray =
        let dr = ray.RayStart - segment.Start
        let d = ray.RayDir |> Vector.cross segment.Dir
        let na = segment.Dir |> Vector.cross dr
        let nb = ray.RayDir |> Vector.cross dr

        if eq0 d then // parallel or coincident lines
            // intersect at segment start, opposite directions
            if ray |> sqDistanceToPoint segment.Start |> eq0 && ray |> sqDistanceToPoint segment.End |> eqE (segmentMag2 segment) then Point segment.Start
            // intersect at segment end, opposite directions
            elif ray |> sqDistanceToPoint segment.End |> eq0 && ray |> sqDistanceToPoint segment.Start |> eqE (segmentMag2 segment) then Point segment.End
            // if the ray hits either segment point then they share a sub-segment
            elif ray |> sqDistanceToPoint segment.Start |> eq0 || ray |> sqDistanceToPoint segment.End |> eq0 then Infinite
            // ray start isn't on the segment and it points away from the segment
            else NoIntersect
        else
            let ua = na / d
            let ub = nb / d
            if geq0 ua && geq0 ub && leqE ub 1.0 then
                Point (ray.RayStart + ua * ray.RayDir)
            else
                NoIntersect
    let sqDistanceToLine line ray = Line.sqDistanceToRay ray line
    let sqDistanceToRay ray2 ray1 =
        match rayIntersection ray2 ray1 with
        | NoIntersect -> min (ray1 |> sqDistanceToPoint ray2.RayStart) (ray2 |> sqDistanceToPoint ray1.RayStart)
        | Infinite | Point _ -> 0.0
    let sqDistanceToSegment segment ray =
        match segmentIntersection segment ray with
        | NoIntersect -> min (sqDistanceToPoint segment.Start ray) (sqDistanceToPoint segment.End ray) |> min (segmentSqDistanceToPoint ray.RayStart segment)
        | Infinite | Point _ -> 0.0

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Segment =
    let init start dir = { Start = start; Dir = dir }
    let toLine = segmentToLine
    let toRay segment = { RayStart = segment.Start; RayDir = segment.Dir }
    let mag2 = segmentMag2
    let mag = mag2 >> sqrt
    let reverse (segment:Segment) = { Start = segment.End; Dir = -segment.Dir }
    let isEquivalent segment2 segment1 = segment1 = segment2 || segment1 = reverse segment2
    let getL v segment =
        let a = v - segment.Start
        (Vector.dot a segment.Dir |> sign |> float) * (Vector.mag2 a / Vector.mag2 segment.Dir |> sqrt)
    let closestPointTo = segmentClosestPointTo
    let sqDistanceToPoint = segmentSqDistanceToPoint
    let sqDistanceToLine line segment = Line.sqDistanceToSegment segment line
    let sqDistanceToRay ray segment = Ray.sqDistanceToSegment segment ray
    let segmentIntersection segment2 segment1 =
        let dr = segment1.Start - segment2.Start
        let d = segment1.Dir |> Vector.cross segment2.Dir
        let na = segment2.Dir |> Vector.cross dr
        
        if eq0 d then
            let distStart = sqDistanceToPoint segment2.Start segment1
            let distEnd = sqDistanceToPoint segment2.End segment1
            let s2mag2 = mag2 segment2
            if eq0 distStart && eqE distEnd s2mag2 then Point segment2.Start
            elif eq0 distEnd && eqE distStart s2mag2 then Point segment2.End
            elif (eq0 distStart || eq0 distEnd) && eq0 na && segment1.Dir |> Vector.cross dr |> eq0 then Infinite
            else NoIntersect
        else
            let s = na / d
            if lt0 s || gtE s 1.0 then NoIntersect
            else
                let p = segment1.Start + s * segment1.Dir
                let r = Vector.dot segment2.Dir (p - segment2.Start) / (Vector.mag2 segment2.Dir)
                if lt0 r || gtE r 1.0 then NoIntersect
                else Point p
    let sqDistanceToSegment segment2 segment1 =
        match segmentIntersection segment2 segment1 with
        | NoIntersect ->
            min
                (min (sqDistanceToPoint segment2.Start segment1) (sqDistanceToPoint segment2.End segment1))
                (min (sqDistanceToPoint segment1.Start segment2) (sqDistanceToPoint segment1.End segment2))
        | Infinite | Point _ -> 0.0
    let lineIntersection line segment = Line.segmentIntersection segment line
    let rayIntersection ray segment = Ray.segmentIntersection segment ray

type Polygon =
    private { vectors : Vector array }
    member this.Item with get i = this.vectors.[i]
    override this.ToString () = sprintf "Polygon(%A)" this.vectors

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Polygon =
    let make points = { vectors = Array.ofSeq points }
    let pointPairs polygon = polygon.vectors |> Seq.circularPairwise
    let pointTriples = pointPairs >> Seq.circularPairwise >> Seq.map (fun ((a, b), (_, c)) -> (a, b, c))
    let count polygon = polygon.vectors.Length
    let area = pointPairs >> Seq.sumBy (fun (p1, p2) -> p1 |> Vector.cross p2) >> (*) 0.5
    let center polygon = Array.average polygon.vectors
    let points polygon = List.ofArray polygon.vectors
    let segments = pointPairs >> Seq.toList >> List.map (fun (p1, p2) -> { Start = p1; Dir = p2 - p1 })
    let contains point polygon =
        if polygon |> segments |> List.exists (Segment.sqDistanceToPoint point >> eq0) then true
        else polygon|> pointPairs |> Seq.sumBy (fun (p1, p2) -> (p1 - point) |> Vector.angleTo (p2 - point)) |> abs |> gt0
    let isClockwise = pointPairs >> Seq.sumBy (fun (p1, p2) -> (p2.x - p1.x) * (p2.y + p1.y)) >> gt0
    let isCounterClockwise = isClockwise >> not
    let isConvex polygon =
        polygon
        |> segments
        |> Seq.circularPairwise
        |> Seq.toList
        |> List.forall (fun (s1, s2) -> s1.Dir |> Vector.cross s2.Dir |> geq0)
        |> (=) (isClockwise polygon)
    let isConcave = isConvex >> not
    let reverse polygon = polygon.vectors |> Array.rev |> make
    let isEquivalentTo polygonB polygonA =
        let length = count polygonA
        if length = count polygonB then
            let idx = [| 0 .. length - 1 |]
            Array.exists
                (fun i -> Array.forall (fun j -> polygonA.[j] = polygonB.[(i + j) % length]) idx)
                idx
        else false
    let shrink distance = // TODO convert to Attempt
        segments
        >> Seq.circularPairwise
        >> Seq.collect
            (fun (left, right) ->
                if left.Dir |> Vector.cross right.Dir |> eq0 then
                    let shift = { Vector.x = -left.Dir.y; y = left.Dir.x } |> Vector.normalize |> (*) distance
                    right.Start + shift ::
                        if left.Dir |> Vector.dot right.Dir |> gt0 then []
                        else [ right.Start - shift ]
                else
                    let shiftLine = Segment.toLine >> Line.shiftLeft distance
                    match left |> shiftLine |> Line.lineIntersection <| shiftLine right with
                    | Point p -> [ p ]
                    | NoIntersect | Infinite -> invalidOp "Unexpected intersection in shrink: intersection was not a point")
        >> make
    let getInsidePoint polygon =
        if isCounterClockwise polygon then
            polygon
            |> pointTriples
            |> Seq.tryPick
                (fun (a, v, b) ->
                    if b - v |> Vector.cross (a - v) |> gt0 then
                        let inside =
                            polygon
                            |> points
                            |> List.filter
                                (fun q ->
                                    (v - a |> Vector.cross (q - a) |> gt0)
                                    && (b - v |> Vector.cross (q - v) |> gt0)
                                    && (a - b |> Vector.cross (q - b) |> gt0))
                        match inside with
                        | [] -> (v + a + b) / 3.0
                        | list -> list |> List.minBy (Vector.sqDistanceTo v) |> (+) v |> (*) 0.5
                        |> Some
                    else None)
        else None
        |> function // TODO make this error go away
            | Some point when contains point polygon -> point
            | _ -> failwith "Unable to get inside point of polygon"
    let hasSelfIntersection polygon =
        let test head tail =
            tail
            |> List.exists
                (fun seg ->
                    match Segment.segmentIntersection head seg with
                    | NoIntersect -> false
                    | Infinite -> true
                    | Point x -> (x <> head.Start && x <> head.End) || (x <> seg.Start && x <> seg.End)
                )
        let rec testIntersection =
            function
            | [] | _ :: [] -> false
            | head :: tail ->
                if test head tail then true
                else testIntersection tail
        polygon |> segments |> testIntersection
    let intersectsPolygon (p1:Polygon) p2 =
        if contains p1.[0] p2 || contains p2.[0] p1 then
            true
        else
            let s2 = segments p2
            p1
            |> segments
            |> List.forall
                (fun s1 ->
                    s2
                    |> List.forall (Segment.segmentIntersection s1 >> (=) NoIntersect))
            |> not
    let private apply (f:Vector seq -> Vector seq) = points >> f >> make
    let private apply2 (f:(Vector * Vector) seq -> Vector seq) = pointPairs >> f >> make
    let private apply3 (f:(Vector * Vector * Vector) seq -> Vector seq) = pointTriples >> f >> make
    let map (mapper:Vector -> Vector) = mapper |> Seq.map |> apply
    let map2 (mapper:Vector * Vector -> Vector) = mapper |> Seq.map |> apply2
    let map3 (mapper:Vector * Vector * Vector -> Vector) = mapper |> Seq.map |> apply3
    let choose (chooser:Vector -> Vector option) = chooser |> Seq.choose |> apply
    let choose2 (chooser:Vector * Vector -> Vector option) = chooser |> Seq.choose |> apply2
    let choose3 (chooser:Vector * Vector * Vector -> Vector option) = chooser |> Seq.choose |> apply3
    let filter (f:Vector -> bool) = f |> Seq.filter |> apply
    
type Circle =
  { Center : Vector
    Radius : float }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Circle =
    let intersectsCircle circleA circleB =
        let minDist = circleA.Radius + circleB.Radius
        Vector.sqDistanceTo circleA.Center circleB.Center <= minDist * minDist
    let intersectsPolygon (polygon:Polygon) circle =
        let r2 = circle.Radius * circle.Radius
        if Vector.sqDistanceTo circle.Center polygon.[0] <= r2 || Polygon.contains circle.Center polygon then
            true
        else
            polygon
            |> Polygon.segments
            |> List.exists
                (fun seg ->
                    let d =
                        seg
                        |> Segment.closestPointTo circle.Center
                        |> Vector.sqDistanceTo circle.Center
                    d <= r2)

let Vec x y = { x = x; y = y }
let Seg s e = { Start = s; Dir = e - s }

let bezier3 (p0:Vector) (p1:Vector) (p2:Vector) t =
    if t < 0.0 || t > 1.0 then invalidArg "t" "t must be in the range [0, 1]"
    let u = 1.0 - t
    u * u * p0 + 2.0 * u * t * p1 + t * t * p2

let bezier4 p0 p1 p2 p3 t =
    if t < 0.0 || t > 1.0 then invalidArg "t" "t must be in the range [0, 1]"
    (1.0 - t) * bezier3 p0 p1 p2 t + t * bezier3 p1 p2 p3 t

let bezier points t =
    if Seq.isEmpty points then invalidArg "points" "Points must contain at least 1 element"
    elif t < 0.0 || t > 1.0 then invalidArg "t" "t must be in the range [0, 1]"
    else
        let rec bezier n (p:Vector seq) =
            if n = 1 then Seq.head p
            else
                p
                |> Seq.pairwise
                |> Seq.map (fun (p0, p1) -> (1.0 - t) * p0 + t * p1)
                |> bezier (n - 1)
        bezier (Seq.length points) points

let convexHull points =
    let p = Array.ofSeq points
    Array.sortInPlaceWith
        (fun a b ->
            if ltE a.x b.x then -1
            elif gtE a.x b.x then 1
            elif ltE a.y b.y then -1
            elif gtE a.y b.y then 1
            else 0)
        p
    let rec addToList v list =
        match list with
        | a :: b :: _ when a - b |> Vector.cross (v - a) |> lt0 -> list |> List.tail |> addToList v
        | _ -> v :: list
    let lower = Array.fold (fun list v -> addToList v list) [] p
    let upper = Array.foldBack addToList p []
    Polygon.make ((lower |> List.tail |> List.rev) @ (upper |> List.tail |> List.rev))
    
let toSegmentGraph segments =
    Graph.make
        (segments |> Seq.collect (fun s -> [ s.Start; s.End ]) |> Seq.distinct)
        (segments |> Seq.collect (fun s -> [ Edge (s.Start, s.End, Segment.mag s); Edge (s.End, s.Start, Segment.mag s) ]))

let toUnweightedSegmentGraph segments =
    Graph.make
        (segments |> Seq.collect (fun s -> [ s.Start; s.End ]) |> Seq.distinct)
        (segments |> Seq.collect (fun s -> [ Edge (s.Start, s.End, 1.0); Edge (s.End, s.Start, 1.0) ]))

let nonIntersectingSegments =
    let intersect a b =
        if Segment.isEquivalent a b then
            Some [ a ]
        else
            match Segment.segmentIntersection a b with
            | NoIntersect -> None
            | Infinite ->
                [| a.Start; a.End; b.Start; b.End |]
                |> Array.sortBy (fun pt -> Segment.getL pt a)
                |> Seq.circularPairwise
                |> Seq.fold
                    (fun list (v1, v2) ->
                        if v1 = v2 then list
                        else Seg v1 v2 :: list)
                    []
                |> List.tail
                |> Some
            | Point p ->
                let p = Vector.round p
                match List.collect (fun s -> if p <> s.Start && p <> s.End then [ Seg s.Start p ; Seg p s.End ] else [ s ] ) [ a; b ]
                    |> List.filter (Segment.mag2 >> gt0) with
                | _ :: _ :: [] -> None
                | any -> Some any
    let enqueue segment queue =
        if PairingHeap.contains segment queue then queue
        else PairingHeap.insert segment (max segment.Start segment.End) queue
    let step (resultSegs, endQueue, startQueue) =
        match (PairingHeap.isEmpty endQueue, startQueue) with
        | (true, []) -> (resultSegs, endQueue, startQueue)
        | (true, h :: t) -> (resultSegs, enqueue h endQueue, t)
        | (false, h :: t) when endQueue |> PairingHeap.minKey > h.Start -> (resultSegs, enqueue h endQueue, t)
        | (false, _) ->
            let seg = PairingHeap.findMin endQueue
            let endQueue = PairingHeap.deleteMin endQueue
            match endQueue |> PairingHeap.values |> Seq.tryPick (fun b -> match intersect seg b with | None -> None | Some segList -> Some (segList, b)) with
            | None -> (seg :: resultSegs, endQueue, startQueue)
            | Some (segList, secondSeg) -> (resultSegs, List.fold (fun q s -> enqueue s q) (PairingHeap.delete secondSeg endQueue) segList, startQueue)
    let rec go (resultSegs, endQueue, startQueue) =
        match PairingHeap.isEmpty endQueue, startQueue with
        | (true, []) -> resultSegs
        | (_, _) -> (resultSegs, endQueue, startQueue) |> step |> go
    fun segments ->
        go ([],
            PairingHeap.empty,
            segments
                |> Seq.map (fun s -> if s.Start > s.End then Segment.reverse s else s)
                |> Seq.distinct
                |> Seq.sortBy (fun s -> s.Start)
                |> List.ofSeq)

let breakGraphIntoPolygons (g:Graph<Vector, _>) =
    let rec onePoly poly0 poly1 (p1, p2, list, used) =
        match list with
        | a :: b :: vecs when a = poly1 && b = poly0 && vecs <> [] ->
            let polygon = List.rev vecs
            let u = polygon |> Seq.circularPairwise |> Seq.fold (fun us t -> Set.add t us) used
            (Polygon.make polygon, u)
        | _ ->
            let v12 = p2 - p1
            let p3 =
                g.[p2]
                |> List.map (fun e -> e.endVertex)
                |> List.filter (fun p3 -> used |> Set.contains (p2, p3) |> not)
                |> List.maxBy
                    (fun p3 ->
                        let angle = v12 |> Vector.angleTo (p3 - p2)
                        if geqE angle PI then -angle
                        else angle)
            onePoly poly0 poly1 (p2, p3, p2 :: list, used)
    let nextPoly (e:Edge<Vector, _>) used =
        let p1 = e.startVertex
        let p2 = e.endVertex
        onePoly p1 p2 (p1, p2, [ p1 ], used)
    Graph.edges g
    |> Seq.fold
        (fun (polygons, used) e ->
            if Set.contains (e.startVertex, e.endVertex) used then
                polygons, used
            else
                let nP, nU = nextPoly e used
                nP :: polygons, nU)
        ([], Set.empty)
    |> fst

let breakIntoPolygons : Segment seq -> Polygon list = toUnweightedSegmentGraph >> breakGraphIntoPolygons

let breakPairsIntoPolygons segments =
    Graph.make
        (segments |> Seq.collect (fun (a, b) -> [ a; b ]) |> Seq.distinct)
        (segments |> Seq.collect (fun (a, b) -> [ Edge (a, b, 1); Edge (b, a, 1) ]))
    |> breakGraphIntoPolygons
    
[<RequireQualifiedAccess>]
module private Triangulation =
    type private Vertex =
        | Start
        | End
        | Split
        | Merge
        | Regular

    type private Chain =
        | Both
        | Left
        | Right

    let private distanceRightOf point (p1, p2) =
        let seg = Seg p1 p2
        let line = Line.init point (Vec 1.0 0.0)
        match Line.segmentIntersection seg line with
        | NoIntersect -> System.Double.MaxValue * 0.5
        | Infinite -> Segment.closestPointTo point seg |> Line.getL <| line
        | Point p -> Line.getL p line

    let private compareSegments point seg1 seg2 =
        let d1 = distanceRightOf point seg1
        let d2 = distanceRightOf point seg2
        if d1 < d2 then -1
        elif d2 < d1 then 1
        else 0

    let rec private tryGetLeftSegment point =
        function
        | Nil -> None
        | Node (v, _, left, right) ->
            let d = distanceRightOf point v
            if d > 0.0 then tryGetLeftSegment point left
            elif d < 0.0 then
                match tryGetLeftSegment point right with
                | Some p -> Some p
                | None -> Some v
            else Some v
    let private segmentOnLeft point treap =
        match tryGetLeftSegment point treap with
        | None -> failwithf "unreachable: no segment on the left of point = %A in treap = %A" point treap
        | Some seg -> seg
        
    let private vectorSort a b =
        if gtE a.y b.y then -1
        elif ltE a.y b.y then 1
        elif ltE a.x b.x then -1
        elif gtE a.x b.x then 1
        else 0

    let triangulateMonotone polygon =
        let adjacent =
            polygon
            |> Polygon.pointTriples
            |> Seq.map (fun (a, b, c) -> (b, (a, c)))
            |> dict
        let U = polygon |> Polygon.points |> List.sortWith vectorSort
        let chain =
            List.fold
                (fun chain v ->
                    if Map.containsKey v chain then chain
                    else
                        let (prev, next) = adjacent.[v]
                        match (Map.tryFind prev chain, Map.tryFind next chain) with
                        | (None, None) -> failwithf "unreachable: no adjacent vertices to v = %A are part of either chain in chain = %A" v chain
                        | (Some Both, None) -> Left
                        | (None, Some Both) -> Right
                        | (Some c, None) | (None, Some c) -> c
                        | (Some _, Some _) -> Both
                        |> Map.add v <| chain)
                (Map.empty |> Map.add (List.head U) Both)
                U
            |> Map.toSeq
            |> dict
        let diagonals =
            match U with
            | u0 :: u1 :: U ->
                let rec f stack points edges =
                    match points with
                    // No points (unreachable)
                    | [] -> edges
                    // Last point, add diagonals from here to every point on the stack except the first and last
                    | last :: [] ->
                        let rec popStack S edges =
                            match S with
                            | [] | _ :: [] -> edges
                            | v :: remain -> popStack remain <| (last, v) :: edges
                        match stack with
                        | [] -> edges
                        | _ :: stack -> popStack stack edges
                    | u :: points ->
                        let top = List.head stack
                        // Points are on different chains
                        if (chain.[u] = Left && chain.[top] = Right) || (chain.[u] = Right && chain.[top] = Left) then
                            // Add diagonals from this point to every point on the stack except the oldest
                            let rec popStack S edges =
                                match S with
                                | [] | _ :: [] -> edges
                                | v :: remain -> popStack remain <| (u, v) :: edges
                            f [ u; top ] points <| popStack stack edges
                        // Points are on the same chain
                        else
                            // Add diagonals as long as they're inside the polygon
                            let rec popInside S lastPopped edges =
                                match S with
                                | v :: remaining ->
                                    let cross = v - u |> Vector.cross (lastPopped - u)
                                    if (gt0 cross && chain.[u] = Left) || (lt0 cross && chain.[u] = Right) then popInside remaining v <| (u, v) :: edges
                                    else (S, lastPopped, edges)
                                | [] -> ([], lastPopped, edges)
                            let (S, lastPopped, edges) = popInside (List.tail stack) top edges
                            f (u :: lastPopped :: S) points edges
                f [ u1; u0 ] U []
            | _ -> failwithf "unreachable: sorted list U contains less than 2 elements, U = %A" U
        diagonals
        |> Seq.append (Polygon.pointPairs polygon)
        |> breakPairsIntoPolygons
        |> Seq.filter Polygon.isCounterClockwise

    let toMonotonePolygons polygon =
        let noHorizontal = Polygon.choose3 (fun (a, b, c) -> if eqE a.y b.y && eqE b.y c.y then None else Some b) polygon
        let adjacent =
            noHorizontal
            |> Polygon.pointTriples
            |> Seq.toList
            |> List.map (fun (a, b, c) -> (b, (a, c)))
            |> dict
        let nextEdge =
            adjacent
            |> Seq.map (fun kv -> (kv.Key, (kv.Key, snd kv.Value)))
            |> dict
        let prevEdge =
            adjacent
            |> Seq.map (fun kv -> (kv.Key, (fst kv.Value, kv.Key)))
            |> dict
        let vertexType =
            adjacent
            |> Seq.map
                (fun kv ->
                    let pt = kv.Key
                    let (prev, next) = kv.Value
                    let angle = (next - pt) |> Vector.angleTo (prev - pt)
                    pt,
                    if leqE prev.y pt.y && ltE next.y pt.y && angle > 0.0 then Start
                    elif ltE prev.y pt.y && leqE next.y pt.y && angle < 0.0 then Split
                    elif geqE prev.y pt.y && gtE next.y pt.y && angle > 0.0 then End
                    elif gtE prev.y pt.y && geqE next.y pt.y && angle < 0.0 then Merge
                    else Regular)
            |> dict
            
        let helper = System.Collections.Generic.Dictionary<Vector * Vector, Vector>()

        let handleStart (T, D) v =
            let e = nextEdge.[v]
            helper.[e] <- v
            (ComparatorTreap.insert (compareSegments v) e T, D)
        let handleEnd (T, D) v =
            let e = prevEdge.[v]
            let h = helper.[e]
            (ComparatorTreap.delete (compareSegments v) e T, if vertexType.[h] = Merge then (v, h) :: D else D)
        let handleSplit (T, D) v =
            let ei = nextEdge.[v]
            let ej = segmentOnLeft v T
            let D = (v, helper.[ej]) :: D
            helper.[ej] <- v
            helper.[ei] <- v
            (ComparatorTreap.insert (compareSegments v) ei T, D)
        let handleMerge (T, D) v =
            let p = prevEdge.[v]
            let h = helper.[p]
            let D = if vertexType.[h] = Merge then (v, h) :: D else D
            let T = ComparatorTreap.delete (compareSegments v) p T
            let ej = segmentOnLeft v T
            let h = helper.[ej]
            let D = if vertexType.[h] = Merge then (v, h) :: D else D
            helper.[ej] <- v
            (T, D)
        let handleRegular (T, D) v =
            let (prev, next) = adjacent.[v]
            if prev.y >= v.y && v.y >= next.y then
                let p = prevEdge.[v]
                let h = helper.[p]
                let D = if vertexType.[h] = Merge then (v, h) :: D else D
                let e = nextEdge.[v]
                let T = T |> ComparatorTreap.delete (compareSegments v) p |> ComparatorTreap.insert (compareSegments v) e
                helper.[e] <- v
                (T, D)
            else
                let ej = segmentOnLeft v T
                let h = helper.[ej]
                let D = if vertexType.[h] = Merge then (v, h) :: D else D
                helper.[ej] <- v
                (T, D)
        let handle state v =
            match vertexType.[v] with
            | Start -> handleStart
            | End -> handleEnd
            | Split -> handleSplit
            | Merge -> handleMerge
            | Regular -> handleRegular
            <| state <| v

        Seq.fold
            handle
            (ComparatorTreap.empty, polygon |> Polygon.pointPairs |> Seq.toList)
            (List.sortWith vectorSort <| Polygon.points noHorizontal)
        |> snd
        |> breakPairsIntoPolygons
        |> List.filter Polygon.isCounterClockwise

let triangulate = Triangulation.toMonotonePolygons >> Seq.collect Triangulation.triangulateMonotone

type VectorTransform =
    | Translate of Vector
    | Rotate of float

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module VectorTransform =
    let apply =
        function
        | Translate translation -> (+) translation
        | Rotate angle -> Vector.rotate angle
    let toFunction transforms =
        transforms
        |> List.fold
            (fun f transform -> f >> apply transform)
            id

type Rectangle =
  { Left : int
    Right : int
    Bottom : int
    Top : int }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Rectangle =
    let init width height = { Left = 0; Right = width; Bottom = 0; Top = height }
    let widthOf rectangle = rectangle.Right - rectangle.Left
    let heightOf rectangle = rectangle.Top - rectangle.Bottom
    let areaOf rectangle = widthOf rectangle * heightOf rectangle
    let toPolygon rectangle =
        let { Left = left; Right = right; Bottom = bottom; Top = top } = rectangle
        Polygon.make
            [|  Vec <| float left  <| float bottom
                Vec <| float right <| float bottom
                Vec <| float right <| float top
                Vec <| float left  <| float top |]
    let notContainedIn container rectangle =
        let x = [ rectangle.Left; rectangle.Right; container.Left; container.Right ] |> Seq.distinct |> Seq.sort |> Seq.pairwise
        let y = [ rectangle.Bottom; rectangle.Top; container.Bottom; container.Top ] |> Seq.distinct |> Seq.sort |> Seq.pairwise
        seq {
            for x1, x2 in x do
                for y1, y2 in y do
                    if (rectangle.Left <= x1 && rectangle.Right > x1 && rectangle.Bottom <= y1 && rectangle.Top > y1) &&
                       not (container.Left <= x1 && container.Right > x1 && container.Bottom <= y1 && container.Top > y1) then
                        yield { Rectangle.Left = x1; Right = x2; Bottom = y1; Top = y2 } }
    let isContainedBy container rectangle =
        container.Left <= rectangle.Left && container.Right >= rectangle.Right && container.Bottom <= rectangle.Bottom && container.Top >= rectangle.Top
    let overlaps r1 r2 =
        not (r2.Right <= r1.Left || r2.Left >= r1.Right || r2.Top <= r1.Bottom || r2.Bottom >= r1.Top)
    // Rotate 90 degrees, keeping the same left & bottom coordinates
    let rotateInPlace rect =
        { rect with Right = rect.Left + heightOf rect; Top = rect.Bottom + widthOf rect }
    // Rotate around the origin by 90 * n degrees
    let rec rotateAroundOrigin n rect =
        if n = 0 then rect
        elif n < 0 then rotateAroundOrigin (n + 4) rect
        else { rect with Left = -rect.Top; Right = -rect.Bottom; Bottom = rect.Left; Top = rect.Right } |> rotateAroundOrigin (n - 1)
    let shiftRight x rect = { rect with Left = rect.Left + x; Right = rect.Right + x }
    let shiftUp y rect = { rect with Bottom = rect.Bottom + y; Top = rect.Top + y }
    let shift x y rect = rect |> shiftRight x |> shiftUp y
    
type Cube =
  { x1 : float
    x2 : float
    y1 : float
    y2 : float
    z1 : float
    z2 : float }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Cube =
    let makeSize x y z =
        if lt0 x then invalidArg "x" "x must be >= 0.0"
        if lt0 y then invalidArg "y" "y must be >= 0.0"
        if lt0 z then invalidArg "z" "z must be >= 0.0"
        { x1 = 0.0
          x2 = x
          y1 = 0.0
          y2 = y
          z1 = 0.0
          z2 = z }

    let make (x1, y1, z1) (x2, y2, z2) =
        { x1 = min x1 x2
          x2 = max x1 x2
          y1 = min y1 y2
          y2 = max y1 y2
          z1 = min z1 z2
          z2 = max z1 z2 }

    let shiftX dx (cube:Cube) =
        { cube with
            x1 = cube.x1 + dx
            x2 = cube.x2 + dx }

    let shiftY dy (cube:Cube) =
        { cube with
            y1 = cube.y1 + dy
            y2 = cube.y2 + dy }

    let shiftZ dz (cube:Cube) =
        { cube with
            z1 = cube.z1 + dz
            z2 = cube.z2 + dz }

    let shift dx dy dz =
        shiftX dx
        >> shiftY dy
        >> shiftZ dz

    let rotateXY (cube:Cube) = // rotate around the line L = c * z (x = 0, y = 0)
        { cube with
            x1 = -cube.y2
            x2 = -cube.y1
            y1 = cube.x1
            y2 = cube.x2 }

    let intersects cube1 cube2 =
        cube1.x2 > cube2.x1 && cube1.x1 < cube2.x2 &&
        cube1.y2 > cube2.y1 && cube1.y1 < cube2.y2 &&
        cube1.z2 > cube2.z1 && cube1.z1 < cube2.z2

    let contains (x, y, z) cube =
        cube.x1 <= x && x <= cube.x2 &&
        cube.y1 <= y && y <= cube.y2 &&
        cube.z1 <= z && z <= cube.z2

    let containedBy container contained =
        container.x1 <= contained.x1 && container.x2 >= contained.x2 &&
        container.y1 <= contained.y1 && container.y2 >= contained.y2 &&
        container.z1 <= contained.z1 && container.z2 >= contained.z2

    let containedByList containers contained =
        let getBounds axis =
            axis contained @ (List.collect axis containers)
            |> Util.coordinateCompression
            |> snd
        let xIndex = getBounds (fun cube -> [ cube.x1; cube.x2 ])
        let yIndex = getBounds (fun cube -> [ cube.y1; cube.y2 ])
        let zIndex = getBounds (fun cube -> [ cube.z1; cube.z2 ])

        let uncovered = Array3D.create xIndex.Count yIndex.Count zIndex.Count false
        let mark value cube =
            for x in xIndex.[cube.x1] .. xIndex.[cube.x2] do
                for y in yIndex.[cube.y1] .. yIndex.[cube.y2] do
                    for z in zIndex.[cube.z1] .. zIndex.[cube.z2] do
                        uncovered.[x, y, z] <- value
        mark true contained
        List.iter (mark false) containers
        
        let rec check x y z =
            if x = xIndex.Count then true
            elif y = yIndex.Count then check (x + 1) 0 0
            elif z = zIndex.Count then check x (y + 1) 0
            elif uncovered.[x, y, z] then false
            else check x y (z + 1)
        check 0 0 0
