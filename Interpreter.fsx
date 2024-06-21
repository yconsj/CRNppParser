#r "CRNppInterpreter/Library/net7.0/CRNppInterpreter.dll"
#r "nuget: Plotly.NET, 4.0.0"

open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling
open Interpreter.Parser


type State = Map<Species, Number>


let getTwoSpecies state s1 s2 =
    let A = Map.find s1 state
    let B = Map.find s2 state
    (A, B)

let ExecuteModule m (state: Map<Species, Number>) =
    match m with
    | ADD(s1, s2, s3) ->
        let (A, B) = getTwoSpecies state s1 s2
        Map.add s3 (A + B) state
    | SUB(s1, s2, s3) ->
        let (A, B) = getTwoSpecies state s1 s2

        if (A > B) then
            Map.add s3 (A - B) state
        else
            Map.add s3 (0.0) state
    | DIV(s1, s2, s3) ->
        let (A, B) = getTwoSpecies state s1 s2
        Map.add s3 (A / B) state
    | MUL(s1, s2, s3) ->
        let (A, B) = getTwoSpecies state s1 s2
        Map.add s3 (A * B) state

    | LD(s1, s2) -> Map.add s1 (Map.find s2 state) state

    | SQRT(s1, s2) -> Map.add s1 (sqrt (Map.find s2 state)) state

    | CMP(s1, s2) ->
        let (v1, v2) = (Map.find s1 state, Map.find s2 state)

        match (v1, v2) with
        | v1, v2 when v1 > v2 -> Map.add "_flag" 3 state
        | v1, v2 when v1 = v2 -> Map.add "_flag" 2 state
        | v1, v2 when v1 < v2 -> Map.add "_flag" 1 state
        | _ -> failwith "compare error between"

let rec step state (stp: StepS) =
    match stp with
    | Module(h) :: t -> step (ExecuteModule h state) t
    | Conditional(h) :: t -> step (ExecuteConditional h state) t
    | [] -> state
    | _ -> state

and ExecuteConditional c (state: Map<Species, Number>) =
    let flag = Map.find "_flag" state

    match c with
    | IfGT(l) when flag = 3 -> step state l
    | IfGE(l) when flag = 3 || flag = 2 -> step state l
    | IfEQ(l) when flag = 2 -> step state l
    | IfLT(l) when flag = 1 -> step state l
    | IfLE(l) when flag = 1 || flag = 2 -> step state l
    | _ -> state

let rec steps state stps =
    match stps with
    | h :: [] -> step state h
    | h :: t -> steps (step state h) t
    | _ -> failwith "error no steps"

let rec getSteps rootList =
    match rootList with
    | Conc(_) :: t -> getSteps t
    | Step(x) :: [] -> [ x ]
    | Step(x) :: t -> x :: getSteps t
    | [] -> []


let rec helper cmdl state =
    match cmdl with
    | Module(MUL(a, b, c)) :: t
    | Module(DIV(a, b, c)) :: t
    | Module(SUB(a, b, c)) :: t
    | Module(ADD(a, b, c)) :: t ->
        let newState = Map.add c 0.0 (Map.add b 0.0 (Map.add a 0 state))
        helper t newState
    | Module(LD(a, b)) :: t
    | Module(SQRT(a, b)) :: t ->
        let newState = (Map.add b 0.0 (Map.add a 0.0 state))
        helper t newState
    | Module(CMP(a, b)) :: t ->
        let newState = Map.add "_flag" 0.0 (Map.add b 0.0 (Map.add a 0.0 state))
        helper t newState
    | Conditional(IfGT(l)) :: t
    | Conditional(IfGE(l)) :: t
    | Conditional(IfEQ(l)) :: t
    | Conditional(IfLE(l)) :: t
    | Conditional(IfLT(l)) :: t ->
        let newState = helper l state
        helper t newState
    | Rxn(x)::t -> failwith "rxn not implemented"
    | [] -> state

let intializeVariables rootList =
    let rec intializeVariables' rootList' state =
        match rootList' with
        | Conc(_) :: t -> intializeVariables' t state
        | Step(x) :: t -> intializeVariables' t (helper x state)
        | [] -> state

    intializeVariables' rootList Map.empty

let rec intializeConcentrations rootList state =
    match rootList with
    | Conc(species, number) :: t -> intializeConcentrations t (Map.add species number state)
    | Step(x) :: t -> intializeConcentrations t state
    | [] -> state


let interpretProgram (programTree: Crn) =

    let (CRN rl) = programTree
    let stps = getSteps rl
    let variables = intializeVariables rl
    let (s0: Map<Species, Number>) = intializeConcentrations rl variables
    printfn "%A \n" stps
    printfn "%A \n" s0
    let length = stps.Length

    Seq.append (seq {s0}) (Seq.unfold
        (fun (index, state) ->
            let newState = step state (List.item (index % length) stps)
            Some(newState, (index + 1, newState)))
        (0, s0))

let testProgramTree =
    CRN[Conc("a", 2.0)
        Conc("b", 1.0)
        Step[Module(ADD("a", "b", "c"))]]



printf "%A" (interpretProgram testProgramTree)
