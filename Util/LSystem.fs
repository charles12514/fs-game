namespace Util.LSystem

open System
open System.Text.RegularExpressions
open Util

module private Regexes =
    let private StrName = "[a-zA-z_][a-zA-Z_0-9]*"
    let private StrFloat = @"[0-9]*\.?[0-9]+"
    let private StrParameterModifier = "(any|prev|previous)"
    let private StrParameter = "(" + StrName + "|" + StrParameterModifier + @"\(" + StrName + @"\))"
    let private StrParameterList = StrParameter + @"(\s+" + StrParameter + ")*"
    let private StrSymbol = StrName + @"(\(" + StrParameterList + @"\))?"
    let private StrLhs = StrSymbol + @"(\s+\[p\s*=\s*" + StrFloat + @"\])?"
    let private StrRhs = StrSymbol + @"(\s+" + StrSymbol + ")*"
    let private StrProduction = StrLhs + @"\s+>\s+" + StrRhs
    let private StrStart = @"start\s+=\s+" + StrRhs
    let private StrLSystem = StrStart + "\n(" + StrProduction + "\n)*"

    let private StartReg = @"^start\s+=\s+(?<symbol>" + StrSymbol + @")(\s+(?<symbol>" + StrSymbol + "))*$"
    let private SymbolReg = "^(?<symbolname>" + StrName + @")(\((?<parameter>" + StrParameter + @")(\s+(?<parameter>" + StrParameter + @"))*\))?$"
    let private ParameterReg = "^((?<name>" + StrName + ")|(?<modifier>" + StrParameterModifier + @")\((?<name>" + StrName + @")\))$"
    let private ProductionReg = "^(?<lhs>" + StrLhs + @")\s+>\s+(?<rhs>" + StrRhs + ")$"
    let private LhsReg = "^(?<symbol>" + StrSymbol + @")(\s+\[p\s*=\s*(?<float>" + StrFloat + @")\])?$"
    let private RhsReg = "^(?<symbol>" + StrSymbol + @")(\s+(?<symbol>" + StrSymbol + "))*$"

    let StartRegex = Regex (StartReg, RegexOptions.Compiled)
    let SymbolRegex = Regex (SymbolReg, RegexOptions.Compiled)
    let ParameterRegex = Regex (ParameterReg, RegexOptions.Compiled)
    let ProductionRegex = Regex (ProductionReg, RegexOptions.Compiled)
    let LhsRegex = Regex (LhsReg, RegexOptions.Compiled)
    let RhsRegex = Regex (RhsReg, RegexOptions.Compiled)

open Regexes

exception LSystemConstructionException of string * string

(* Parameter
 *      S -> name
 *      S -> any(name)
 *      S -> prev(name)
 *      S -> previous(name)
 *)
type Parameter =
  { ParamName : string
    Previous : bool }

    member this.Any = not this.Previous
    override this.ToString () =
        if this.Any then sprintf "any(%s)" this.ParamName
        else this.ParamName

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Parameter =
    let parse str =
        let par = ParameterRegex.Match str
        if par.Success then
            let name = par.Groups.["name"].Value
            let previous = (par.Groups.["modifier"].Success && par.Groups.["modifier"].Value = "any") |> not
            { ParamName = name; Previous = previous }
        else
            LSystemConstructionException ("Invalid parameter", str) |> raise

(* Symbol
 *      S -> name
 *      S -> name(P)
 *      P -> Parameter
 *      P -> Parameter P
 *)
[<NoComparison>]
type Symbol =
  { SymbolName : string
    Parameters : Parameter seq }

    override this.ToString () =
        if this.Parameters |> Seq.isEmpty then this.SymbolName
        else this.SymbolName + "(" + (Seq.fold (fun str p -> str + p.ToString () + " ") "" this.Parameters).Trim () + ")"

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Symbol =
    let parse str =
        let sym = SymbolRegex.Match str
        if sym.Success then
            let name = sym.Groups.["symbolname"].Value
            let parameters =
                if sym.Groups.["parameter"].Success then
                    sym.Groups.["parameter"].Captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun p -> p.ToString () |> Parameter.parse)
                else Seq.empty
            { SymbolName = name; Parameters = parameters }
        else
            LSystemConstructionException ("Invalid symbol", str) |> raise

(* Production
 *      S -> L > R
 *      L -> Symbol
 *      L -> Symbol [p=number]
 *      R -> Symbol
 *      R -> Symbol R
 *)
[<NoComparison>]
type Production =
  { Lhs : Symbol
    Rhs : Symbol seq
    Probability : float }

    override this.ToString () =
        let prob =
            if this.Probability < 0.0 then ""
            else sprintf " [p=%.3f]" this.Probability
        let right = Seq.fold (fun str sym -> str + " " + sym.ToString ()) "" this.Rhs
        sprintf "%s%s >%s" (this.Lhs.ToString ()) prob right

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Production =
    let canApply symbol production = production.Lhs.SymbolName = symbol.SymbolName
    let parse line =
        let whole = ProductionRegex.Match line
        if whole.Success then
            let left = LhsRegex.Match whole.Groups.["lhs"].Value
            let lhs = Symbol.parse left.Groups.["symbol"].Value
            if Seq.length lhs.Parameters <= 1 then
                let p =
                    if left.Groups.["float"].Success then
                        match System.Double.TryParse left.Groups.["float"].Value with
                        | (false, _) ->
                            LSystemConstructionException (sprintf "Invalid production; probability %s is not a float" left.Groups.["float"].Value, line) |> raise
                        | (true, f) ->
                            if f < 0.0 || f > 1.0 then
                                LSystemConstructionException (sprintf "Invalid production; probablity %f is outside the range [0, 1]" f, line) |> raise
                            else f
                    else -1.0

                let right = RhsRegex.Match whole.Groups.["rhs"].Value
                let rhs = right.Groups.["symbol"].Captures |> Seq.cast<Capture> |> Seq.map (fun s -> Symbol.parse(s.ToString ()))
                { Lhs = lhs; Rhs = rhs; Probability = p }
            else
                LSystemConstructionException ("Invalid production; too many parameters on LHS", line) |> raise
        else
            LSystemConstructionException ("Invalid production", line) |> raise

    (* Apply this production to symbol
     * Returns Some list of symbols on the RHS of this production, with any parameter substitution
     * Returns None if this production cannot be applied to symbol
     *)
    let apply symbol production =
        if canApply symbol production then
            // Get LHS parameter name for substitution
            let substitute =
                if Seq.isEmpty production.Lhs.Parameters then Seq.singleton
                else
                    let name = (Seq.head production.Lhs.Parameters).ParamName
                    (fun p -> if name = p.ParamName then symbol.Parameters else Seq.singleton p) // Substitute parameters
            // Copy RHS symbols
            production.Rhs
            |> Seq.map (fun sym -> { sym with Parameters = Seq.collect substitute sym.Parameters })
            |> Some
        else None

[<NoComparison>]
type LSystem =
  { Axiom : Symbol seq
    Productions : Production seq }
    
    override this.ToString () =
        let axiom = Seq.fold (fun acc sym -> acc + " " + sym.ToString ()) "start =" this.Axiom
        Seq.fold (fun acc pr -> acc + System.Environment.NewLine + pr.ToString ()) axiom this.Productions

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module LSystem =
    // Parse an L-System from several lines
    let load (s:string seq) =
        if Seq.isEmpty s then { Axiom = Seq.empty; Productions = Seq.empty }
        else
            let axiom =
                let startTest = StartRegex.Match <| (Seq.head s).Trim().ToLower()
                if startTest.Success then
                    startTest.Groups.["symbol"].Captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun sym -> sym.ToString () |> Symbol.parse)
                else LSystemConstructionException ("Invalid axiom", Seq.head s) |> raise
            let productions =
                s
                |> Seq.skip 1
                |> Seq.map (fun line -> line.Trim().ToLower() |> Production.parse)
                |> Seq.groupBy (fun pr -> pr.Lhs.SymbolName)
                |> Seq.collect
                    (fun group -> // Assign probabilities to unassigned productions
                        let sameName = snd group
                        let sum = sameName |> Seq.sumBy (fun pr -> max 0.0 pr.Probability)
                        if sum > 1.0 then LSystemConstructionException (sprintf "Invalid LSystem; productions for %s have combined probability > 1" (fst group), "") |> raise
                        let remain = (1.0 - sum) / (sameName |> Seq.filter (fun pr -> pr.Probability < 0.0) |> Seq.length |> float)
                        sameName
                        |> Seq.map
                            (fun pr ->
                                if pr.Probability < 0.0 then { pr with Probability = remain }
                                else pr))
                |> Seq.filter (fun pr -> pr.Probability > 0.0) // Remove unusable productions
            productions
            |> Seq.groupBy (fun pr -> pr.Lhs.SymbolName)
            |> Seq.iter
                (fun group ->
                    if group |> snd |> Seq.sumBy (fun pr -> pr.Probability) |> neqE 1.0 then
                        LSystemConstructionException (sprintf "Invalid LSystem; productions for %s have combined probability != 1" (fst group), "") |> raise)
            { Axiom = axiom; Productions = productions }
        
    // Parse an L-System from a file
    let loadFile filename = filename |> System.IO.File.ReadAllLines |> load

    let private run isComplete selectRuleFor lsystem =
        // Apply productions to each symbol
        let next state =
            let rules = Seq.map (selectRuleFor lsystem) state
            if Seq.forall Option.isNone rules then (state, true)
            else
                let n =
                    Seq.map2
                        (fun sym ->
                            function
                            | Some p ->
                                match Production.apply sym p with
                                | Some s -> s
                                | None -> invalidOp <| sprintf "Unable to apply production %A to symbol %A" p sym
                            | None -> Seq.singleton sym)
                        state
                        rules
                    |> Seq.collect id
                (n, false)
        // Run to completion
        let rec go symbols ((count, _) as fin) =
            if isComplete fin then symbols
            else
                let (nextSymbols, term) = next symbols
                go nextSymbols (count + 1, term)
        go lsystem.Axiom (0, false)
    let private firstAvailableProduction lsystem symbol = lsystem.Productions |> Seq.tryFind (Production.canApply symbol)
    let private randomProduction (rng:Random) lsystem symbol =
        let rules = lsystem.Productions |> Seq.filter (Production.canApply symbol)
        match Seq.length rules with
        | 0 -> None                      // No productions
        | 1 -> rules |> Seq.head |> Some // 1 possible production with p=1
        | _ ->                           // Choose from 2+ possible productions
            // Pick a weighted random production
            rules
            |> Seq.fold
                (fun state pr ->
                    match state with
                    | (_, Some _) -> state
                    | (r, None) when r <= pr.Probability -> (0.0, Some pr)
                    | (r, None) -> (r - pr.Probability, None))
                (rng.NextDouble (), None)
            |> snd
            |> function
               | Some pr -> Some pr
               | None -> rules |> Seq.head |> Some
    let private nStepsOrTerm n (completedSteps, term) =
        if term then true
        else completedSteps >= n
    let private term = snd

    let runDeterministic iterations = run (nStepsOrTerm iterations) firstAvailableProduction
    let runNondeterministic iterations rng = run (nStepsOrTerm iterations) (randomProduction rng)
    let runDeterministicToCompletion = run term firstAvailableProduction
    let runNondeterministicToCompletion rng = run term (randomProduction rng)
