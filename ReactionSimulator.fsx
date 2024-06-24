module Sim

#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open MathNet.Numerics
open MathNet.Numerics.OdeSolvers
open MathNet.Numerics.LinearAlgebra
open Interpreter.Parser
open Interpreter.Plotter


type State = Map<Species, Number>

open System


// old simulator
let odes (reactions: RxnS list) (species: Species list) (t: float) (concs: Vector<float>) =
    let concDict =
        Map.ofList [ for i in 0 .. species.Length - 1 -> species.[i], concs.[i] ]

    let countMatchesList el li =
        li |> (Seq.filter (fun x -> x = el) >> Seq.length)

    let netChange sp lhs rhs =
        let lhsCount = countMatchesList sp lhs
        let rhsCount = countMatchesList sp rhs
        double (rhsCount - lhsCount)

    let reactantProduct (concs: State) (reactants: Expr) =
        List.fold (fun acc r -> concs.[r] * acc) 1.0 reactants

    let reactionChangeSpeciesFunc sp concs (reaction: RxnS) =
        match reaction with
        | (lhs, rhs, rate) -> rate * (netChange sp lhs rhs) * reactantProduct concs lhs

    let sumChangeSpeciesFunc species concs reactions =
        reactions |> List.sumBy (reactionChangeSpeciesFunc species concs)

    Vector.Build.DenseOfEnumerable [ for sp in species -> sumChangeSpeciesFunc sp concDict reactions ]

// Function to solve the ODEs for one time step
let solveStep (initialConcs: Vector<float>) (reactions: RxnS list) (species: Species list) (timeStepSize: float) =
    let odeFunc (t: float) (concs: Vector<float>) = odes reactions species t concs
    let resolution = 5
    RungeKutta.SecondOrder(initialConcs, 0.0, timeStepSize, resolution, odeFunc)

// Infinite sequence of states using ODE solver
let reactionSimulator (initialConcs: State) (reactions: RxnS list) (timeStepSize: float) =
    let species = List.ofSeq initialConcs.Keys

    let initialConcsVector =
        Vector<float>.Build.Dense([| for sp in species -> initialConcs.[sp] |])

    let arrayToState (species: Species list) (concs: Vector<float>) =
        let stateList =
            List.zip species (List.ofArray (concs.AsArray()))
            |> List.map (fun (sp, conc) -> sp, conc)

        Map.ofList stateList

    Seq.unfold
        (fun stateVector ->
            let newStateArray = solveStep stateVector reactions species timeStepSize
            let newStateVector = newStateArray.[newStateArray.Length - 1] // Take the last state in the array
            Some(arrayToState species newStateVector, newStateVector))
        initialConcsVector

let reactionSimulatorPlot (initialConcs: State) (reactions: RxnS list) (timeStepSize: float) (timeSteps: int) =
    // timeResolution: how detailed the values are generated. value of >0
    // timeStep: Total number of time units to calculate the state. Value of >1
    // the total number of data points will be = timeStep / timeResolution
    let nDataPoints = int (float timeSteps / timeStepSize)

    let yData: State seq =
        Seq.take nDataPoints (reactionSimulator initialConcs reactions timeStepSize)

    let xData = [ 0.0 .. timeStepSize .. timeSteps ]
    smoothSimPlot xData yData


// let fig1MulConcs : State  = Map([("A",6.0);("B",2.0);("C",0.0);
//     ("D",0.0);
//     ])
// let fig1MulReaction : RxnS list = [
//     RxnS(["A";"B"],["A";"B";"C"],1);
//     RxnS(["C"],[],1);
//     ]

// let fig1States = (reactionSimulator fig1MulConcs fig1MulReaction 0.05)
// simulationPlot (fig1States) 1000


let fig4OscConcs: State = Map([ ("X_1", 2); ("X_2", 0.01); ("X_3", 0.005) ])

let fig4OscReaction: RxnS list =
    [ RxnS([ "X_1"; "X_2" ], [ "X_2"; "X_2" ], 1.0)
      RxnS([ "X_2"; "X_3" ], [ "X_3"; "X_3" ], 1.0)
      RxnS([ "X_3"; "X_1" ], [ "X_1"; "X_1" ], 1.0) ]

//let fig4States = (reactionSimulator fig4OscConcs fig4OscReaction 1)
//simulationPlot (fig4States) 100


//reactionSimulatorPlot fig4OscConcs fig4OscReaction 0.5 100

/// step[]
/// step[... cmp ...]
/// -> virtual step for second part of cmp
/// step[
///
///     ifGT[XgtY&clockspecies
///     // XgtY & XltY
///     // [XgtY] * [XltY]
///  ]
/// ]
///
/// // 4 * 3 clock species
/// // 4 * 3 clock species
/// // 4 * 3 clock species
/// // 4 * 3 clock species
/// // 4 * 3 clock species
/// // 4 * 3 clock species
/// // 4 * 3 clock species
/// // 4 * 3 clock species
/// // 4 * 3 clock species
