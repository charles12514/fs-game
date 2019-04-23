module Util.Simplex

let initialSeed = 0L

let private StretchConstant = -1.0 / 6.0  //(1/sqrt(3+1)-1)/3
let private SquishConstant = 1.0 / 3.0    //(sqrt(3+1)-1)/3
let private NormConstant = 103.0

//Gradients for 3D. They approximate the directions to the
//vertices of a rhombicuboctahedron from the center, skewed so
//that the triangular and square facets can be inscribed inside
//circles of the same radius.
let private Gradients =
    [| -11y;  4y;  4y; -4y;  11y;  4y; -4y;  4y;  11y;
        11y;  4y;  4y;  4y;  11y;  4y;  4y;  4y;  11y;
       -11y; -4y;  4y; -4y; -11y;  4y; -4y; -4y;  11y;
        11y; -4y;  4y;  4y; -11y;  4y;  4y; -4y;  11y;
       -11y;  4y; -4y; -4y;  11y; -4y; -4y;  4y; -11y;
        11y;  4y; -4y;  4y;  11y; -4y;  4y;  4y; -11y;
       -11y; -4y; -4y; -4y; -11y; -4y; -4y; -4y; -11y;
        11y; -4y; -4y;  4y; -11y; -4y;  4y; -4y; -11y |]

//Initializes the class using a permutation array generated from a 64-bit seed.
//Generates a proper permutation (i.e. doesn't merely perform N successive pair swaps on a base array)
//Uses a simple 64-bit LCG.
let private Perm, PermGradIndex =
    let source = [| 0s .. 255s |]
    let updateSeed seed = seed * 6364136223846793005L + 1442695040888963407L
    Array.scan
        (fun (_, seed) i -> (i, updateSeed seed))
        (255, initialSeed |> updateSeed |> updateSeed |> updateSeed |> updateSeed)
        [| 254 .. -1 .. 0 |]
    |> Array.map
        (fun (i, seed) ->
            let r =
                let x = (seed + 31L) % (int64 i + 1L) |> int
                if x < 0 then x + i + 1
                else x
            let perm = source.[r]
            let grad = ((int perm) % 24) * 3 |> int16
            source.[r] <- source.[i]
            (perm, grad))
    |> Array.rev
    |> Array.unzip
    
let private Extrapolate xsb ysb zsb dx dy dz =
    let index = xsb |> (&&&) 0xFF |> Array.get Perm |> int |> (+) ysb |> (&&&) 0xFF |> Array.get Perm |> int |> (+) zsb |> (&&&) 0xFF |> Array.get PermGradIndex |> int
    float Gradients.[index] * dx + float Gradients.[index + 1] * dy + float Gradients.[index + 2] * dz
    
let private FastFloor x =
    let xi = int x
    if x < float xi then xi - 1
    else xi

let private applyTriple (a, b, c) f = (f a, f b, f c)

//3D OpenSimplex (Simplectic) Noise.
let Eval x y z =
    //Place input coordinates on simplectic honeycomb.
    let stretchOffset = (x + y + z) * StretchConstant
    let (xs, ys, zs) as sTriple = applyTriple (x, y, z) ((+) stretchOffset)
    
    //Floor to get simplectic honeycomb coordinates of rhombohedron (stretched cube) super-cell origin.
    let (xsb, ysb, zsb) as sbTriple = applyTriple sTriple FastFloor

    //Skew out to get actual coordinates of rhombohedron origin. We'll need these later.
    let squishOffset = (xsb + ysb + zsb |> float) * SquishConstant
    let (xb, yb, zb) = applyTriple sbTriple (float >> (+) squishOffset)

    //Compute simplectic honeycomb coordinates relative to rhombohedral origin.
    let xins = xs - float xsb
    let yins = ys - float ysb
    let zins = zs - float zsb

    //Sum those together to get a value that determines which region we're in.
    let inSum = xins + yins + zins

    //Positions relative to origin point.
    let dx0 = x - xb
    let dy0 = y - yb
    let dz0 = z - zb

    let (value, ((xsvExt0, xsvExt1, dxExt0, dxExt1), (ysvExt0, ysvExt1, dyExt0, dyExt1), (zsvExt0, zsvExt1, dzExt0, dzExt1))) =
        let contribution a b c dx dy dz =
            let x = 2.0 - dx * dx - dy * dy - dz * dz
            if x > 0.0 then
                let attn = x * x
                attn * attn * Extrapolate (xsb + a) (ysb + b) (zsb + c) dx dy dz
            else 0.0
        if inSum <= 1.0 then
            //We're inside the tetrahedron (3-Simplex) at (0,0,0)
        
            //Determine which two of (0,0,1), (0,1,0), (1,0,0) are closest.
            let (aPoint, aScore, bPoint, bScore) =
                if xins >= yins && zins > yins then  (1uy, xins, 4uy, zins)
                elif xins < yins && zins > xins then (4uy, zins, 2uy, yins)
                else                                 (1uy, xins, 2uy, yins)

            //Now we determine the two lattice points not part of the tetrahedron that may contribute.
            //This depends on the closest two tetrahedral vertices, including (0,0,0)
            let wins = 1.0 - inSum
            let ext =
                if wins > aScore || wins > bScore then
                    //(0,0,0) is one of the closest two tetrahedral vertices.
                    let c = if bScore > aScore then bPoint else aPoint //Our other closest vertex is the closest out of a and b.
                    let xExt =
                        if c &&& 1uy = 0uy then (xsb - 1, xsb, dx0 + 1.0, dx0)
                        else (xsb + 1, xsb + 1, dx0 - 1.0, dx0 - 1.0)
                    let yExt =
                        match (c &&& 2uy, c &&& 1uy) with
                        | (0uy, 0uy) -> (ysb, ysb - 1, dy0, dy0 + 1.0)
                        | (0uy, _) -> (ysb - 1, ysb, dy0 + 1.0, dy0)
                        | (_, _) -> (ysb + 1, ysb + 1, dy0 - 1.0, dy0 - 1.0)
                    let zExt =
                        if c &&& 4uy = 0uy then (zsb, zsb - 1, dz0, dz0 + 1.0)
                        else (zsb + 1, zsb + 1, dz0 - 1.0, dz0 - 1.0)
                    (xExt, yExt, zExt)
                else
                    //(0,0,0) is not one of the closest two tetrahedral vertices.
                    let c = aPoint ||| bPoint //Our two extra vertices are determined by the closest two.
                    let getExt mask sb d =
                        if c &&& mask = 0uy then (sb, sb - 1, d - 2.0 * SquishConstant, d + 1.0 - SquishConstant)
                        else (sb + 1, sb + 1, d - 1.0 - 2.0 * SquishConstant, d - 1.0 - SquishConstant)
                    let xExt = getExt 1uy xsb dx0
                    let yExt = getExt 2uy ysb dy0
                    let zExt = getExt 4uy zsb dz0
                    (xExt, yExt, zExt)

            let dxSquish = dx0 - SquishConstant
            let dySquish = dy0 - SquishConstant
            let dzSquish = dz0 - SquishConstant
            let c000 = contribution 0 0 0 dx0 dy0 dz0
            let c100 = contribution 1 0 0 (dxSquish - 1.0) dySquish dzSquish
            let c010 = contribution 0 1 0 dxSquish (dySquish - 1.0) dzSquish
            let c001 = contribution 0 0 1 dxSquish dySquish (dzSquish - 1.0)
            
            (c000 + c100 + c010 + c001, ext)
        elif inSum >= 2.0 then
            //We're inside the tetrahedron (3-Simplex) at (1,1,1)

            //Determine which two tetrahedral vertices are the closest, out of (1,1,0), (1,0,1), (0,1,1) but not (1,1,1).
            let (aPoint, aScore, bPoint, bScore) =
                if xins <= yins && zins < yins then  (6uy, xins, 3uy, zins)
                elif xins > yins && zins < xins then (3uy, zins, 5uy, yins)
                else                                 (6uy, xins, 5uy, yins)

            //Now we determine the two lattice points not part of the tetrahedron that may contribute.
            //This depends on the closest two tetrahedral vertices, including (1,1,1)
            let wins = 3.0 - inSum
            let ext =
                if wins < aScore || wins < bScore then
                    //(1,1,1) is one of the closest two tetrahedral vertices.
                    let c = if bScore < aScore then bPoint else aPoint //Our other closest vertex is the closest out of a and b.
                    let xExt =
                        if c &&& 1uy = 0uy then (xsb, xsb, dx0 - 3.0 * SquishConstant, dx0 - 3.0 * SquishConstant)
                        else (xsb + 2, xsb + 1, dx0 - 2.0 - 3.0 * SquishConstant, dx0 - 1.0 - 3.0 * SquishConstant)
                    let yExt =
                        if c &&& 2uy = 0uy then (ysb, ysb, dy0 - 3.0 * SquishConstant, dy0 - 3.0 * SquishConstant)
                        elif c &&& 1uy = 0uy then (ysb + 2, ysb + 1, dy0 - 2.0 - 3.0 * SquishConstant, dy0 - 1.0 - 3.0 * SquishConstant)
                        else (ysb + 1, ysb + 2, dy0 - 1.0 - 3.0 * SquishConstant, dy0 - 2.0 - 3.0 * SquishConstant)
                    let zExt =
                        if c &&& 4uy = 0uy then (zsb, zsb, dz0 - 3.0 * SquishConstant, dz0 - 3.0 * SquishConstant)
                        else (zsb + 1, zsb + 2, dz0 - 1.0 - 3.0 * SquishConstant, dz0 - 2.0 - 3.0 * SquishConstant)
                    (xExt, yExt, zExt)
                else
                    //(1,1,1) is not one of the closest two tetrahedral vertices.
                    let c = aPoint &&& bPoint //Our two extra vertices are determined by the closest two.
                    let getExt mask sb d =
                        if c &&& mask = 0uy then (sb, sb, d - SquishConstant, d - 2.0 * SquishConstant)
                        else (sb + 1, sb + 2, d - 1.0 - SquishConstant, d - 2.0 - 2.0 * SquishConstant)
                    let xExt = getExt 1uy xsb dx0
                    let yExt = getExt 2uy ysb dy0
                    let zExt = getExt 4uy zsb dz0
                    (xExt, yExt, zExt)

            let squishX2P1 = 1.0 + 2.0 * SquishConstant
            let squishX3P1 = 1.0 + 3.0 * SquishConstant
            let dxSquish = dx0 - squishX2P1
            let dySquish = dy0 - squishX2P1
            let dzSquish = dz0 - squishX2P1
            let c110 = contribution 1 1 0 dxSquish dySquish (dzSquish + 1.0)
            let c101 = contribution 1 0 1 dxSquish (dySquish + 1.0) dzSquish
            let c011 = contribution 0 1 1 (dxSquish + 1.0) dySquish dzSquish
            let c111 = contribution 1 1 1 (dx0 - squishX3P1) (dy0 - squishX3P1) (dz0 - squishX3P1)

            (c110 + c101 + c011 + c111, ext)
        else
            //We're inside the octahedron (Rectified 3-Simplex) in between.

            //Decide between point (0,0,1) and (1,1,0) as closest
            let (aScore, aPoint, aIsFurtherSide) =
                let p1 = xins + yins
                if p1 > 1.0 then (p1 - 1.0, 3uy, true)
                else             (1.0 - p1, 4uy, false)

            //Decide between point (0,1,0) and (1,0,1) as closest
            let (bScore, bPoint, bIsFurtherSide) =
                let p2 = xins + zins
                if p2 > 1.0 then (p2 - 1.0, 5uy, true)
                else             (1.0 - p2, 2uy, false)

            //The closest out of the two (1,0,0) and (0,1,1) will replace the furthest out of the two decided above, if closer.
            let (aPoint, aIsFurtherSide, bPoint, bIsFurtherSide) =
                let p3 = yins + zins
                if p3 > 1.0 then
                    let score = p3 - 1.0
                    if aScore <= bScore && aScore < score then  (6uy, true, bPoint, bIsFurtherSide)
                    elif aScore > bScore && bScore < score then (aPoint, aIsFurtherSide, 6uy, true)
                    else                                        (aPoint, aIsFurtherSide, bPoint, bIsFurtherSide)
                else
                    let score = 1.0 - p3
                    if aScore <= bScore && aScore < score then  (1uy, false, bPoint, bIsFurtherSide)
                    elif aScore > bScore && bScore < score then (aPoint, aIsFurtherSide, 1uy, false)
                    else                                        (aPoint, aIsFurtherSide, bPoint, bIsFurtherSide)
                
            //Where each of the two closest points are determines how the extra two vertices are calculated.
            let ((xsvExt0, ysvExt0, zsvExt0, dxExt0, dyExt0, dzExt0), (xsvExt1, ysvExt1, zsvExt1, dxExt1, dyExt1, dzExt1)) =
                match (aIsFurtherSide, bIsFurtherSide) with
                | (true, true) ->
                    //Both closest points on (1,1,1) side

                    //One of the two extra points is (1,1,1)
                    let ext0 = (xsb + 1, ysb + 1, zsb + 1, dx0 - 1.0 - 3.0 * SquishConstant, dy0 - 1.0 - 3.0 * SquishConstant, dz0 - 1.0 - 3.0 * SquishConstant)

                    //Other extra point is based on the shared axis.
                    let c = aPoint &&& bPoint
                    let ext1 =
                        if c &&& 1uy <> 0uy then (xsb + 2, ysb, zsb, dx0 - 2.0 - 2.0 * SquishConstant, dy0 - 2.0 * SquishConstant, dz0 - 2.0 * SquishConstant)
                        elif c &&& 2uy <> 0uy then (xsb, ysb + 2, zsb, dx0 - 2.0 * SquishConstant, dy0 - 2.0 - 2.0 * SquishConstant, dz0 - 2.0 * SquishConstant)
                        else (xsb, ysb, zsb + 2, dx0 - 2.0 * SquishConstant, dy0 - 2.0 * SquishConstant, dz0 - 2.0 - 2.0 * SquishConstant)
                    (ext0, ext1)
                | (false, false) ->
                    //Both closest points on (0,0,0) side

                    //One of the two extra points is (0,0,0)
                    let ext0 = (xsb, ysb, zsb, dx0, dy0, dz0)

                    //Other extra point is based on the omitted axis.
                    let c = aPoint &&& bPoint
                    let ext1 =
                        if c &&& 1uy = 0uy then (xsb - 1, ysb + 1, zsb + 1, dx0 + 1.0 - SquishConstant, dy0 - 1.0 - SquishConstant, dz0 - 1.0 - SquishConstant)
                        elif c &&& 2uy = 0uy then (xsb + 1, ysb - 1, zsb + 1, dx0 - 1.0 - SquishConstant, dy0 + 1.0 - SquishConstant, dz0 - 1.0 - SquishConstant)
                        else (xsb + 1, ysb + 1, zsb - 1, dx0 - 1.0 - SquishConstant, dy0 - 1.0 - SquishConstant, dz0 + 1.0 - SquishConstant)
                    (ext0, ext1)
                | (_, _) ->
                    //One point on (0,0,0) side, one point on (1,1,1) side
                    let (c1, c2) = if aIsFurtherSide then (aPoint, bPoint) else (bPoint, aPoint)

                    //One contribution is a permutation of (1,1,-1)
                    let ext0 =
                        if c1 &&& 1uy = 0uy then (xsb - 1, ysb + 1, zsb + 1, dx0 + 1.0 - SquishConstant, dy0 - 1.0 - SquishConstant, dz0 - 1.0 - SquishConstant)
                        elif c1 &&& 2uy = 0uy then (xsb + 1, ysb - 1, zsb + 1, dx0 - 1.0 - SquishConstant, dy0 + 1.0 - SquishConstant, dz0 - 1.0 - SquishConstant)
                        else (xsb + 1, ysb + 1, zsb - 1, dx0 - 1.0 - SquishConstant, dy0 - 1.0 - SquishConstant, dz0 + 1.0 - SquishConstant)
                    //One contribution is a permutation of (0,0,2)
                    let ext1 =
                        if c2 &&& 1uy <> 0uy then (xsb + 2, ysb, zsb, dx0 - 2.0 - 2.0 * SquishConstant, dy0 - 2.0 * SquishConstant, dz0 - 2.0 * SquishConstant)
                        elif c2 &&& 2uy <> 0uy then (xsb, ysb + 2, zsb, dx0 - 2.0 * SquishConstant, dy0 - 2.0 - 2.0 * SquishConstant, dz0 - 2.0 * SquishConstant)
                        else (xsb, ysb, zsb + 2, dx0 - 2.0 * SquishConstant, dy0 - 2.0 * SquishConstant, dz0 - 2.0 - 2.0 * SquishConstant)
                    (ext0, ext1)
            
            let dxSquish = dx0 - SquishConstant
            let dySquish = dy0 - SquishConstant
            let dzSquish = dz0 - SquishConstant
            let c100 = contribution 1 0 0 (dxSquish - 1.0) dySquish dzSquish
            let c010 = contribution 0 1 0 dxSquish (dySquish - 1.0) dzSquish
            let c001 = contribution 0 0 1 dxSquish dySquish (dzSquish - 1.0)
            let squishX2P1 = 1.0 + 2.0 * SquishConstant
            let dxSquish = dx0 - squishX2P1
            let dySquish = dy0 - squishX2P1
            let dzSquish = dz0 - squishX2P1
            let c110 = contribution 1 1 0 dxSquish dySquish (dzSquish + 1.0)
            let c101 = contribution 1 0 1 dxSquish (dySquish + 1.0) dzSquish
            let c011 = contribution 0 1 1 (dxSquish + 1.0) dySquish dzSquish

            (c100 + c010 + c001 + c110 + c101 + c011, ((xsvExt0, xsvExt1, dxExt0, dxExt1), (ysvExt0, ysvExt1, dyExt0, dyExt1), (zsvExt0, zsvExt1, dzExt0, dzExt1)))

    let contribution a b c dx dy dz =
        let x = 2.0 - dx * dx - dy * dy - dz * dz
        if x > 0.0 then
            let attn = x * x
            attn * attn * Extrapolate a b c dx dy dz
        else 0.0
    let v0 = contribution xsvExt0 ysvExt0 zsvExt0 dxExt0 dyExt0 dzExt0
    let v1 = contribution xsvExt1 ysvExt1 zsvExt1 dxExt1 dyExt1 dzExt1

    (value + v0 + v1) / NormConstant

let create seed levels initialScale =
    let rec noise level scale value x y =
        if level < 1 then value
        else noise (level >>> 1) (scale * 2.0) (value + (Eval (x * scale) (y * scale) seed) / float level) x y
    noise (1 <<< (levels - 1)) initialScale 0.0
