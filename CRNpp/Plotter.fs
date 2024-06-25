namespace CRNpp

module Plotter =
    open Parser
    open Interpreter
    open Plotly.NET
    open Plotly.NET.LayoutObjects

    let genSimulationPlot xData (simData: State seq) (smooth: bool) =
        let rec helperFunc (species: Species) (data: State seq) =
            match Seq.toList data with
            | [] -> seq []
            | h :: tail ->
                let sq1 = seq { (Map.find species h) }
                Seq.append sq1 (helperFunc species tail)

        let rec helperFunc2 keys xdata ydata smooth =
            match keys with
            | h :: tail ->
                if (smooth) then
                    [ Chart.Spline(xdata, (helperFunc h ydata), false, Smoothing = 0.4, Name = h) ]
                    @ (helperFunc2 tail xdata ydata smooth)
                else
                    [ Chart.Scatter(xdata, (helperFunc h ydata), mode = StyleParam.Mode.Lines, Name = h) ]
                    @ (helperFunc2 tail xdata ydata smooth)
            | [] -> []

        let keys = List.ofSeq (Seq.head simData).Keys
        let charts = helperFunc2 (keys) xData simData smooth
        charts |> Chart.combine

    let genSimulationPlotNSteps (simData: seq<State>) nSteps =
        let nData = Seq.take nSteps simData
        let x = [ 0..1..nSteps ]
        genSimulationPlot x nData

    let simulationPlotWithTitle (simData: seq<State>) nSteps title =
        let title = Title.init (title)

        Chart.withTitle title (genSimulationPlotNSteps simData nSteps false)
        |> Chart.show

    let simulationPlot (simData: seq<State>) nSteps =
        simulationPlotWithTitle simData nSteps ""

    let smoothSimPlot (xData) (simData: seq<State>) =
        (genSimulationPlot xData simData true) |> Chart.show
