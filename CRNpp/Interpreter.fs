// @author Simon Janum
// @author August Valentin
// @date 26/6/2024
namespace CRNpp

module Interpreter =
    open Parser
    open TypeChecker

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

        | LD(s1, s2) -> Map.add s2 (Map.find s1 state) state

        | SQRT(s1, s2) -> Map.add s2 (sqrt (Map.find s1 state)) state

        | CMP(s1, s2) ->
            let (v1, v2) = (Map.find s1 state, Map.find s2 state)

            match (v1, v2) with
            | v1, v2 when v1 > v2 -> Map.add "_flag" 3 state
            | v1, v2 when v1 = v2 -> Map.add "_flag" 2 state
            | v1, v2 when v1 < v2 -> Map.add "_flag" 1 state
            | _ -> failwith "Compare error between"


    let rec step state (stp: StepS) =
        match stp with
        | Module(h) :: t -> step (ExecuteModule h state) t
        | Conditional(h) :: t -> step (ExecuteConditional h state) t
        | Rxn(_) :: t -> failwith "Interpreter does not support Rxn"
        | [] -> state

    and ExecuteConditional c (state: Map<Species, Number>) =
        let flag = Map.find "_flag" state

        match c with
        | IfGT(l) when flag = 3 -> step state l
        | IfGE(l) when flag = 3 || flag = 2 -> step state l
        | IfEQ(l) when flag = 2 -> step state l
        | IfLT(l) when flag = 1 -> step state l
        | IfLE(l) when flag = 1 || flag = 2 -> step state l
        | _ when flag = 0 -> state
        | _ -> state



    let rec getSteps rootList =
        match rootList with
        | Conc(x) :: t -> getSteps t
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
        | Conditional(IfGE(l)) :: t
        | Conditional(IfGT(l)) :: t
        | Conditional(IfEQ(l)) :: t
        | Conditional(IfLE(l)) :: t
        | Conditional(IfLT(l)) :: t ->
            let newState = helper l state
            helper t newState
        | Rxn(_) :: t -> failwith "Interpreter does not support Rxn"
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


    let interpretProgram (programAST) =


        let (CRN rl) = programAST

        match typeChecker (CRN rl) with
        | Some(x) -> failwith x
        | None ->
            let variables = intializeVariables rl
            let (s0: Map<Species, Number>) = intializeConcentrations rl variables
            let stps = getSteps rl
            let length = stps.Length

            let s =
                Seq.unfold
                    (fun (index, state) ->
                        let newState = step state (List.item (index % length) stps)
                        Some(newState, ((index + 1), newState)))
                    (0, s0)

            Seq.append (Seq.singleton s0) s

    let parseAndInterpret program = interpretProgram (parseCrn program)
