// @author Simon Janum
// @author August Valentin
// @date 26/6/2024
#r "CRNpp\\Library\\net7.0\\CRNpp.dll"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open CRNpp.Parser
open CRNpp.Simulator
open Plotly.NET
open Plotly.NET.TraceObjects
open Plotly.NET.LayoutObjects

type State = Map<Species, Number>

if false then
    let AddErrorPlots =
        let x' = [ 0.0..1.0..60.0 ]
        let y' = x'

        let addError x y =
            let concs: State = Map([ ("A", x); ("B", y); ("C", 0.0) ])

            let reactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "C" ], 1)
                  RxnS([ "B" ], [ "B"; "C" ], 1)
                  RxnS([ "C" ], [], 1) ]

            let timeStepSize = 0.5
            let steps = 250.0
            let s = reactionSimulator concs reactions timeStepSize
            let lastMap = Seq.item (int (steps / timeStepSize)) s
            let c = Map.find "C" lastMap

            abs (c - (x + y))

        let z' = x' |> List.map (fun x -> (y' |> List.map (fun y -> addError x y)))

        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.7, Contours = Contours.initXyz (Show = true))

        let zExpAxis = LinearAxis.init (TickFormat = ".2e")

        surface |> Chart.withZAxis zExpAxis

    AddErrorPlots |> Chart.show
    printfn "addplot"

if true then
    let MulErrorPlots =
        let x' = [ 0.0..1.0..60.0 ]
        let y' = x'

        let mulError x y =
            let concs: State = Map([ ("A", x); ("B", y); ("C", 0.0) ])

            let reactions: RxnS list =
                [ RxnS([ "A"; "B" ], [ "A"; "B"; "C" ], 1); RxnS([ "C" ], [], 1) ]


            let timeStepSize = 0.5
            let steps = 250.0
            let s = reactionSimulator concs reactions timeStepSize
            let lastMap = Seq.item (int (steps / timeStepSize)) s
            let c = Map.find "C" lastMap

            abs (c - (x * y))

        let z' = x' |> List.map (fun x -> (y' |> List.map (fun y -> mulError x y)))

        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.7, Contours = Contours.initXyz (Show = true))

        let zExpAxis = LinearAxis.init (TickFormat = ".2e")

        surface |> Chart.withZAxis zExpAxis

    MulErrorPlots |> Chart.show
    printfn "mulplot"

if true then
    let SubErrorPlots =
        let x' = [ 0.0..1.0..60.0 ]
        let y' = x'


        let subError x y =
            let concs: State = Map([ ("A", x); ("B", y); ("C", 0.0); ("H", 0.0) ])

            let reactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "C" ], 1)
                  RxnS([ "B" ], [ "B"; "H" ], 1)
                  RxnS([ "C" ], [], 1)
                  RxnS([ "C"; "H" ], [], 1) ]


            let timeStepSize = 0.01
            let steps = 2.5
            let s = reactionSimulator concs reactions timeStepSize
            let lastMap = Seq.item (int (steps / timeStepSize)) s
            let c = Map.find "C" lastMap

            if x > y then abs (c - (x - y)) else abs (c)

        let z' = x' |> List.map (fun x -> (y' |> List.map (fun y -> subError x y)))

        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.7, Contours = Contours.initXyz (Show = true))

        let zExpAxis = LinearAxis.init (TickFormat = ".2e")

        surface |> Chart.withZAxis zExpAxis

    SubErrorPlots |> Chart.show
    printfn "subplot"

if true then
    let divErrorPlots =
        let x' = [ 0.0..1.0..60.0 ]
        let y' = x'

        let divError x y =
            let concs: State = Map([ ("A", x); ("B", y); ("C", 0.0) ])

            let reactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "C" ], 1) ] @ [ RxnS([ "B"; "C" ], [ "B" ], 1) ]

            let timeStepSize = 0.5
            let steps = 250.0
            let s = reactionSimulator concs reactions timeStepSize
            let lastMap = Seq.item (int (steps / timeStepSize)) s
            let a = Map.find "A" lastMap
            let b = Map.find "B" lastMap
            let c = Map.find "C" lastMap
            abs (c - (x / y))

        let z' = x' |> List.map (fun x -> (y' |> List.map (fun y -> divError x y)))

        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.7, Contours = Contours.initXyz (Show = true))

        let zExpAxis = LinearAxis.init (TickFormat = ".2e")

        surface |> Chart.withZAxis zExpAxis

    divErrorPlots |> Chart.show
    printfn "divplot"

if true then
    let sqrtErrorPlots =
        let x' = [ 0.0..1.0..60.0 ]

        let sqrtError x =
            let concs: State = Map([ ("A", x); ("B", 0) ])

            let reactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "B" ], 1) ] @ [ RxnS([ "B"; "B" ], [], 0.5) ]

            let timeStepSize = 0.5
            let steps = 250.0
            let s = reactionSimulator concs reactions timeStepSize
            let lastMap = Seq.item (int (steps / timeStepSize)) s
            let a = Map.find "A" lastMap
            let b = Map.find "B" lastMap
            abs (b - (sqrt x))

        let z' = x' |> List.map (fun x -> sqrtError x)
        let line = Chart.Scatter(x = x', y = z', mode = StyleParam.Mode.Lines)

        let yExpAxis = LinearAxis.init (TickFormat = ".2e")

        line |> Chart.withYAxis yExpAxis

    sqrtErrorPlots |> Chart.show
    printfn "sqrtplot"

if true then
    let LdErrorPlots =
        let x' = [ 0.0..1.0..60.0 ]

        let ldError x =
            let concs: State = Map([ ("A", x); ("B", 0); ("zero", 0) ])

            let reactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "B" ], 1) ] @ [ RxnS([ "B" ], [], 1) ]
            // let reactions: RxnS list =
            //     [ RxnS([ "A" ], [ "A"; "B" ], 1)
            //       RxnS([ "zero" ], [ "zero"; "B" ], 1)
            //       RxnS([ "B" ], [], 1) ]
            let timeStepSize = 0.5
            let steps = 250.0
            let s = reactionSimulator concs reactions timeStepSize
            let lastMap = Seq.item (int (steps / timeStepSize)) s
            let a = Map.find "A" lastMap
            let b = Map.find "B" lastMap
            abs (b - (x))

        let z' = x' |> List.map (fun x -> ldError x)

        let line = Chart.Scatter(x = x', y = z', mode = StyleParam.Mode.Lines)
        let yExpAxis = LinearAxis.init (TickFormat = ".2e")

        line |> Chart.withYAxis yExpAxis

    LdErrorPlots |> Chart.show
    printfn "ldplot"
