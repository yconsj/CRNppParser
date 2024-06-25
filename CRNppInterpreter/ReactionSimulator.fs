namespace Interpreter

module Simulator =
    open MathNet.Numerics.OdeSolvers
    open MathNet.Numerics.LinearAlgebra
    open Parser
    open Plotter


    type State = Map<Species, Number>
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

    let floatFloor input floor =
        if input < floor then floor else input

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
                let newStateVector = newStateVector.Map (fun x -> floatFloor x 0.0 ) // dont allow negative concentrations
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
