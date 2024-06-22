namespace Interpreter

module Plotter =
    open Parser
    open Interpreter
    open Plotly.NET
    open Plotly.NET.LayoutObjects

    let genSimulationPlot (simData: seq<State>) nSteps =

        let rec helperFunc (species: Species) (data: State seq) =
            match Seq.toList data with
            | [] -> seq []
            | h :: tail ->
                let sq1 = seq { (Map.find species h) }
                Seq.append sq1 (helperFunc species tail)

        let rec helperFunc2 keys xdata ydata =
            match keys with
            | h :: tail ->
                [ Chart.Scatter(xdata, (helperFunc h ydata), mode = StyleParam.Mode.Lines, Name = h) ]
                @ (helperFunc2 tail xdata ydata)
            | [] -> []

        let nData = Seq.take nSteps simData
        let x = [ 0..1 .. Seq.length nData ]
        let keys = List.ofSeq (Seq.head nData).Keys
        let charts = helperFunc2 (keys) x nData
        charts |> Chart.combine

    let simulationPlot (simData : seq<State>) nSteps =
        (genSimulationPlot simData nSteps) |> Chart.show;
