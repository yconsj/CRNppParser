#r "CRNppInterpreter/Library/net7.0/CRNppInterpreter.dll"
#r "nuget: Plotly.NET, 4.0.0"

open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling
open Interpreter.Parser


type State = Map<Species, Number>


let getThreeSpecies state s1 s2 s3 =
    let A = Map.find s1 state
    let B = Map.find s2 state
    let C = Map.find s3 state

    (A, B, C)

let ExecuteModule m (state: Map<Species, Number>) =
    match m with
    | ADD(s1, s2, s3) ->
        let (A, B, C) = getThreeSpecies state s1 s2 s3
        Map.add s3 (A + B + C) state
    | SUB(s1, s2, s3) ->
        let (A, B, C) = getThreeSpecies state s1 s2 s3
        Map.add s3 (A - B - C) state
    | CMP(s1,s2) -> 
        let (v1,v2) = (Map.find s1 state, Map.find s2 state)
        match (v1,v2) with
        | v1,v2 when v1 > v2 -> Map.add "_flag" 3 state
        | v1, v2 when v1 = v2 -> Map.add "_flag" 2 state
        | v1, v2 when v1 < v2 -> Map.add "_flag" 1 state
        | _ -> failwith "compare error between"
    | _ -> state

let ExecuteConditional c (state: Map<Species, Number>) =
    match c with
    | _ -> state

let rec step state (stp: StepS) =
    match stp with
    | Module(h) :: [] -> ExecuteModule h state
    | Module(h) :: t -> step (ExecuteModule h state) t
    | Conditional(h) :: [] -> ExecuteConditional h state
    | Conditional(h) :: t -> step (ExecuteConditional h state) t
    | _ -> state

let rec steps state stps =
    match stps with
    | h :: [] -> step state h
    | h :: t -> steps (step state h) t
    | _ -> failwith "error no steps"
let rec getSteps rootList =
    match rootList with
    | Conc(x) :: t -> getSteps t
    | Step(x) :: [] -> [x]
    | Step(x) :: t -> x :: getSteps t
    | [] -> []
    | _ -> failwith "Error getsteps"


let rec helper cmdl state =
    match cmdl with
    | Module(MUL(a,b,c))::t
    | Module(DIV(a,b,c))::t
    | Module(SUB(a,b,c))::t
    | Module(ADD(a,b,c))::t -> 
        let newState = Map.add c 0.0 (Map.add b 0.0(Map.add a 0 state))
        helper t newState
    | Module(LD(a,b))::t
    | Module(SQRT(a,b))::t ->
        let newState = (Map.add b 0.0(Map.add a 0.0 state))
        helper t newState
    | Module(CMP(a,b))::t -> 
        let newState = Map.add "_flag" 0.0 (Map.add b 0.0(Map.add a 0.0 state))
        helper t newState
    | Conditional(IfGE(l))::t
    | Conditional(IfEQ(l))::t
    | Conditional(IfGE(l))::t
    | Conditional(IfGT(l))::t -> let newState = helper l state
                                 helper t newState
    | [] -> state
let intializeVariables rootList  =
    let rec intializeVariables' rootList' state =
        match rootList' with 
        | Conc(_) :: t  -> intializeVariables' t state
        | Step(x) :: t  -> intializeVariables' t (helper x state)
        | [] -> state

    intializeVariables' rootList Map.empty

let rec intializeConcentrations rootList state =
    match rootList with
    | Conc(species, number) :: t    -> intializeConcentrations t (Map.add species number state)
    | Step(x) :: t                  -> intializeConcentrations t state
    | [] -> state
    | _ -> failwith "Error Intilize concentrations"


let CrnProgram (programTree: Crn) =

    let (CRN rl) = programTree
    let stps = getSteps rl
    let variables = intializeVariables rl
    let (s0: Map<Species, Number>) = intializeConcentrations rl variables
    printfn "%A \n" stps
    printfn "%A \n" s0
    Seq.unfold
        (fun state ->
            let newState = steps state stps
            Some(newState, newState))
        s0


let testProgramTree=  CRN[Conc("a",2.0); Conc("b",1.0);Step[Module (ADD("a","b","c"))]]



printf "%A" (CrnProgram testProgramTree)



