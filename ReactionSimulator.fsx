#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"
open Interpreter.Parser
open Interpreter.Plotter


type State = Map<Species, Number>

let nextStep (currentConcs : State) (reactions : RxnS list) (timeRes : Number) : State =
    let countMatchesList el li = li |> (Seq.filter (fun x -> x=el) >> Seq.length) 
    let netChange sp lhs rhs =
        let lhsCount = countMatchesList sp lhs
        let rhsCount = countMatchesList sp rhs
        double (rhsCount - lhsCount)

    let reactantProduct (concs : State) (reactants : Expr) = 
        List.fold (fun acc r -> 
            let rConc = Map.find r concs
            rConc * acc) 1.0 reactants
        
    let reactionChangeSpeciesFunc sp concs (reaction : RxnS) =
        match reaction with
        | (lhs, rhs, rate) -> rate * (netChange sp lhs rhs) * reactantProduct concs lhs
        
    
    let rec sumChangeSpeciesFunc species concs reactions =
        match reactions with
        | r::tail -> 
            (reactionChangeSpeciesFunc species concs r) + 
            (sumChangeSpeciesFunc species concs tail)
        | [] -> 0.0


    let rec helperFunc (speciesKeyList: Species list) (prevConcs : State) (newState : State) (reactions : RxnS list)= 
        match speciesKeyList with
        | sp::tail -> 
            let spChange = (sumChangeSpeciesFunc sp prevConcs reactions) * timeRes
            let newSpValue = (Map.find sp prevConcs) + spChange
            let changeMap = Map.add sp newSpValue newState
            helperFunc tail prevConcs changeMap reactions
        | [] -> newState
    let spKeys = List.ofSeq (currentConcs.Keys)
    helperFunc spKeys currentConcs Map.empty reactions


// main function: take initial concentration, list of reactions and a time resolution
// produce an infinite sequence of states
let reactionSimulator (initialConcs : State) (reactions: RxnS list) timeRes =
     Seq.append (seq {initialConcs}) (Seq.unfold
        (fun (state) ->
            let newState = (nextStep state reactions timeRes)
            Some(newState, newState))
        (initialConcs))

let exampleAddConcs : State = Map([("A",2.0);("B",2.0);("C",0.0)])
let exampleAddReactions : RxnS list = [RxnS(["A"],["A";"C"],1);
    RxnS(["B"],["B";"C"],1);
    RxnS(["C"],[],1)]


let exampleAdd = List.ofSeq (Seq.take 1000 (reactionSimulator exampleAddConcs exampleAddReactions 0.1))
// printfn "%A" exampleAdd
// printfn "%A" (List.last exampleAdd)

let exampleSubConcs : State = Map([("A",5.0);("B",2.0);("C",0.0);("H",0.0)])
let exampleSubReactions : RxnS list = [RxnS(["A"],["A";"C"],1);
    RxnS(["B"],["B";"H"],1);
    RxnS(["C"],[],1);
    RxnS(["C";"H"],[],1)]

let exampleSub = List.ofSeq (Seq.take 1000 (reactionSimulator exampleSubConcs exampleSubReactions 0.1))
// printfn "%A" exampleSub
// printfn "%A" (List.last exampleSub)
// simulationPlot (reactionSimulator exampleSubConcs exampleSubReactions 0.1) 1000

let exampleSqrtConcs : State = Map([("A",144.0);("B",0.0);])
let exampleSqrtReactions : RxnS list = [RxnS(["A"],["A";"B"],1);
    RxnS(["B";"B"],[],0.5)]

let exampleSqrt = List.ofSeq (Seq.take 1000 (reactionSimulator exampleSqrtConcs exampleSqrtReactions 0.001))
simulationPlot (exampleSqrt) 1000

let exampleSqrtAddConcs : State  = Map([("A",100.0);("B",44.0);("C",0.0);
    ("D",0.0);
    ])
let exampleSqrtAddReactions : RxnS list = [
    RxnS(["A"],["A";"C"],1);
    RxnS(["B"],["B";"C"],1);
    RxnS(["C"],[],1);
    
    RxnS(["C"],["C";"D"],1);
    RxnS(["D";"D"],[],0.5)
    ]
let exampleSqrtAdd = (reactionSimulator exampleSqrtAddConcs exampleSqrtAddReactions 0.01)
simulationPlot (exampleSqrtAdd) 1000