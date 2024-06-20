#r "CRNppInterpreter/Library/net7.0/CRNppInterpreter.dll"

open Interperter.Parser


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

let rec getSteps rootList =
    match rootList with
    | Conc(x) :: t -> getSteps t
    | Step(x) :: t -> x :: getSteps t
    | _ -> failwith "Error getsteps"

let intializeConcentrations rootList =
    let rec intializeConcentrations' rootList' state =
        match rootList with
        | Conc(species, number) :: [] -> (Map.add species number state)
        | Conc(species, number) :: t -> intializeConcentrations' t (Map.add species number state)
        | Step(x) :: t -> intializeConcentrations' t state
        | _ -> failwith "Error Intilize concentrations"

    intializeConcentrations' rootList Map.empty

let CrnProgram (programTree: Crn) =



    let (CRN rl) = programTree
    let stps = getSteps rl
    let (s0: Map<Species, Number>) = intializeConcentrations rl

    Seq.unfold
        (fun state ->
            let newState = steps state stps
            Some(newState, newState))
        s0
