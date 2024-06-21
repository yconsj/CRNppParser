#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
open Interpreter.Parser


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

let exampleConcs : State = Map([("A",2.0);("B",2.0);("C",0.0)])
let exampleReactions : RxnS list = [RxnS(["A"],["A";"C"],1);
    RxnS(["B"],["B";"C"],1);
    RxnS(["C"],[],1)]


let example = List.ofSeq (Seq.take 100 (reactionSimulator exampleConcs exampleReactions 0.01))
printfn "%A" example