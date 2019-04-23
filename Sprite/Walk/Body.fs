namespace Walk

open System
open System.Drawing
open System.Numerics

// conventions:
// x = +right, -left
// y = +up, -down
// z = +in, -out
// (0, 0, 0) is directly between character's feet
// character faces +z

[<NoComparison>]
type Leg =
  { Hip : Vector3
    Knee : Vector3
    Foot : Vector3 }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Leg =
    let toShape leg =
        [ Line.make leg.Hip leg.Knee Color.Black
          Line.make leg.Knee leg.Foot Color.Black ]
    let reflectX leg =
        { Hip = Vector3.reflectX leg.Hip
          Knee = Vector3.reflectX leg.Knee
          Foot = Vector3.reflectX leg.Foot }
    let translate translation leg =
        { Hip = leg.Hip + translation
          Knee = leg.Knee + translation
          Foot = leg.Foot + translation }
    let toList leg =
        [ leg.Hip; leg.Knee; leg.Foot ]

[<NoComparison>]
type Arm =
  { Shoulder : Vector3
    Elbow : Vector3
    Hand : Vector3 }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Arm =
    let toShape arm =
        [ Line.make arm.Shoulder arm.Elbow Color.Black
          Line.make arm.Elbow arm.Hand Color.Black ]
    let reflectX arm =
        { Shoulder = Vector3.reflectX arm.Shoulder
          Elbow = Vector3.reflectX arm.Elbow
          Hand = Vector3.reflectX arm.Hand }
    let translate translation arm =
        { Shoulder = arm.Shoulder + translation
          Elbow = arm.Elbow + translation
          Hand = arm.Hand + translation }
    let toList arm =
        [ arm.Shoulder; arm.Elbow; arm.Hand ]

[<NoComparison>]
type Head =
  { Neck : Vector3
    Width : float32
    Height : float32 }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Head =
    let toShape head =
        let x = Vector3.make (head.Width * 0.5f) 0.0f 0.0f
        let y = Vector3.make 0.0f head.Height 0.0f
        [ Line.make (head.Neck - x) (head.Neck + x) Color.Black
          Line.make (head.Neck + x) (head.Neck + x + y) Color.Black
          Line.make (head.Neck + x + y) (head.Neck - x + y) Color.Black
          Line.make (head.Neck - x + y) (head.Neck - x) Color.Black ]
    let translate translation head =
        { head with Neck = head.Neck + translation }

[<NoComparison>]
type Body =
  { LeftLeg : Leg
    RightLeg : Leg
    LeftArm : Arm
    RightArm : Arm
    Head : Head }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Body =
    let Pi = Util.Constants.PI

    let waistWidth = 6.0
    let legLength = 20.0
    let thighLength = legLength * 0.5
    let shinLength = legLength - thighLength
    let torsoHeight = 10.0
    let armLength = 17.0
    let upperArmLength = armLength * 0.5
    let foreArmLength = armLength - upperArmLength
    let shoulderWidth = 9.0
    let headHeight = 7.0
    let headWidth = 5.0

    let walkDepthVariance = 8.0
    let walkHeightVariance = 2.0
    let legAngleToArmAngle = 0.5
    let elbowAngle = Pi / 10.0

    let standing =
        let legX = waistWidth * 0.5 |> float32
        let leftLeg =
            { Hip = Vector3 (-legX, float32 legLength, 0.0f)
              Knee = Vector3 (-legX, float32 shinLength, 0.0f)
              Foot = Vector3 (-legX, 0.0f, 0.0f) }
        let shoulderX = shoulderWidth * 0.5 |> float32
        let shoulderHeight = legLength + torsoHeight |> float32
        let leftArm =
            { Shoulder = Vector3 (-shoulderX, shoulderHeight, 0.0f)
              Elbow = Vector3 (-shoulderX, shoulderHeight - float32 upperArmLength, 0.0f)
              Hand = Vector3 (-shoulderX, shoulderHeight - float32 armLength, 0.0f) }
        let head =
            { Neck = Vector3 (0.0f, shoulderHeight, 0.0f)
              Width = float32 headWidth
              Height = float32 headHeight }
        { LeftLeg = leftLeg
          RightLeg = Leg.reflectX leftLeg
          LeftArm = leftArm
          RightArm = Arm.reflectX leftArm
          Head = head }

    let toShape body : Shape =
        let recolor color = List.map (fun line -> { line with Color = color })
        Line.make body.LeftLeg.Hip body.RightLeg.Hip Color.Violet
        :: Line.make body.LeftArm.Shoulder body.RightArm.Shoulder Color.Violet
        :: Line.make body.LeftLeg.Hip body.LeftArm.Shoulder Color.Red
        :: Line.make body.RightLeg.Hip body.RightArm.Shoulder Color.Blue
        :: (Leg.toShape body.LeftLeg |> recolor Color.Red)
        @ (Leg.toShape body.RightLeg |> recolor Color.Blue)
        @ (Arm.toShape body.LeftArm |> recolor Color.Red)
        @ (Arm.toShape body.RightArm |> recolor Color.Blue)
        @ (Head.toShape body.Head |> recolor Color.Violet)

    let translate translation body =
        { LeftLeg = Leg.translate translation body.LeftLeg
          RightLeg = Leg.translate translation body.RightLeg
          LeftArm = Arm.translate translation body.LeftArm
          RightArm = Arm.translate translation body.RightArm
          Head = Head.translate translation body.Head }

    let placeOnFloor body =
        let minY =
            Leg.toList body.LeftLeg
            @ Leg.toList body.RightLeg
            @ Arm.toList body.LeftArm
            @ Arm.toList body.RightArm
            |> List.map (fun v -> v.Y)
            |> List.min
        let translation = Vector3 (0.0f, -minY, 0.0f)
        translate translation body

    let createWalkCycle standing =
        let ellipsePoints a b =
            let integral f a b =
                let delta = (b - a) / 200.0
                [ a .. delta .. b ]
                |> Seq.pairwise
                |> Seq.sumBy (fun (t1, t2) -> (f t1 + f t2) * delta * 0.5)
            let makeEllipseIntegral rx ry =
                fun t ->
                    rx * rx * sin t * sin t + ry * ry * cos t * cos t |> sqrt
            let ellipse = makeEllipseIntegral a b
            let quadrantArc = integral ellipse 0.0 (Pi * 0.5)
            let targetArcLen = quadrantArc * 0.5
            let t =
                let rec findMidpoint min max =
                    let mid = (max + min) / 2.0
                    if max - min < 0.001 then
                        mid
                    else
                        let arc = integral ellipse 0.0 mid
                        if arc > targetArcLen then
                            findMidpoint min mid
                        else
                            findMidpoint mid max
                findMidpoint 0.0 (Pi * 0.5)
            let x = a * Math.Cos t
            let y = b * Math.Sin t
            [ (-x, y); (-a, 0.0); (-x, -y); (0.0, -b); (x, -y); (a, 0.0); (x, y); (0.0, b) ]
        let adjustLeg =
            let findKnee hip foot =
                let lenX =
                    let len = (Vector3.Distance (hip, foot) |> float) - Util.Constants.E
                    if len > thighLength + shinLength then thighLength + shinLength
                    else len
                let angle = (lenX * lenX + thighLength * thighLength - shinLength * shinLength) / (2.0 * lenX * thighLength) |> Math.Acos
                let quat = Quaternion.CreateFromYawPitchRoll (0.0f, float32 angle, 0.0f)
                Vector3.Transform (foot - hip, quat)
                |> Vector3.Normalize
                |> (*) (float32 thighLength)
                |> (+) hip
            fun footOffset leg ->
                let foot = leg.Foot + footOffset
                { leg with
                    Foot = foot
                    Knee = findKnee leg.Hip foot }
        let adjustArm leg shoulder =
            let thigh = leg.Knee - leg.Hip
            let upperArm =
                Vector3 (thigh.X, thigh.Y, float32 -legAngleToArmAngle * thigh.Z)
                |> Vector3.Normalize
                |> (*) (float32 upperArmLength)
            let elbowQuat = Quaternion.CreateFromYawPitchRoll (0.0f, float32 elbowAngle, 0.0f)
            let foreArm = Vector3.Transform (Vector3 (0.0f, float32 -foreArmLength, 0.0f), elbowQuat)
            { Shoulder = shoulder
              Elbow = shoulder + upperArm
              Hand = shoulder + upperArm + foreArm }
        let offsets =
            ellipsePoints walkDepthVariance walkHeightVariance
            |> List.map (fun (z, y) -> Vector3 (0.0f, float32 y, float32 z))
        let half = offsets.Length / 2
        let lowerHips =
            let crouch = Vector3 (0.0f, float32 -walkHeightVariance, 0.0f)
            translate crouch standing
        offsets
        |> List.mapi
            (fun index leftOffset ->
                let rightOffset = offsets.[(index + half) % offsets.Length]
                let adjustedLegs =
                    { lowerHips with
                        LeftLeg = adjustLeg leftOffset lowerHips.LeftLeg
                        RightLeg = adjustLeg rightOffset lowerHips.RightLeg }
                let adjustedArms =
                    { adjustedLegs with
                        LeftArm = adjustArm adjustedLegs.LeftLeg adjustedLegs.LeftArm.Shoulder
                        RightArm = adjustArm adjustedLegs.RightLeg adjustedLegs.RightArm.Shoulder }
                adjustedArms
                |> placeOnFloor)
