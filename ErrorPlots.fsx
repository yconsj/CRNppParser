
#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open Interpreter.Parser
open Interpreter.Simulator
open Plotly.NET
open Plotly.NET.TraceObjects
open Plotly.NET.LayoutObjects


type State = Map<Species, Number>

if true then
    let AddErrorPlots =
        let x' = [0.0 .. 1.0 .. 100.0]
        let y' = x'

        let addError x y =
            let addConcs : State = Map([("A",x);("B",y);("C",0.0)])
            let addReactions : RxnS list = [RxnS(["A"],["A";"C"],1);
                RxnS(["B"],["B";"C"],1);
                RxnS(["C"],[],1)]
        
            let s = reactionSimulator addConcs addReactions 0.1
            let lastMap = Seq.item 250 s
            let c = Map.find "C" lastMap
            
            abs (c - (x + y))
        let z' = 
            x' |> List.map (fun x -> 
                ( y' |> List.map (fun y -> addError x y ) )
            )
        printfn "%A" z'
        let contours = (Contours.initXyz( Show = true ))
        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.5, Contours = contours)
        let zExpAxis = LinearAxis.init(TickFormat =".1e")

        surface |> Chart.withZAxis zExpAxis

    AddErrorPlots |> Chart.show 

if false then
    let MulErrorPlots =
        let x' = [0.0 .. 1.0 .. 100.0]
        let y' = x'

        let mulError x y =
            let concs : State = Map([("A",x);("B",y);("C", 0.0)])
            let reactions : RxnS list = [RxnS(["A";"B"],["A";"B";"C"],1);
                RxnS(["C"],[],1)]
        
            let s = reactionSimulator concs reactions 0.1
            let lastMap = Seq.item 100 s
            let c = Map.find "C" lastMap
            
            abs (c - (x * y))

        let z' = 
            x' |> List.map (fun x -> 
                ( y' |> List.map (fun y -> mulError x y ) )
            )
        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.5, Contours = Contours.initXyz (Show = true))

        surface

    MulErrorPlots |> Chart.show 

if false then
    let SubErrorPlots =
        let x' = [0.0 .. 1.0 .. 60.0]
        let y' = x'

         
        let subError x y =
            let subConcs : State = Map([("A",x);("B",y);("C",0.0);("H",0.0)])
            let subReactions : RxnS list = [RxnS(["A"],["A";"C"],1);
                RxnS(["B"],["B";"H"],1);
                RxnS(["C"],[],1);
                RxnS(["C";"H"],[],1)]
            let s = reactionSimulator subConcs subReactions 0.01
            let lastMap = Seq.item 250 s
            //let lastMap = getNthState subConcs subReactions 0.01 250
            let c = Map.find "C" lastMap
            
            if x > y then
                abs (c - (x - y))
            else
                abs(c)

        let z' = 
            x' |> List.map (fun x -> 
                ( y' |> List.map (fun y -> subError x y ) )
            )
        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.5, Contours = Contours.initXyz (Show = true))
        surface
    SubErrorPlots |> Chart.show 

if false then
    let divErrorPlots =
        let x' = [ 0.0..1.0..100.0 ]
        let y' = x'

        let divError x y =
            let divConcs: State = Map([ ("A", x); ("B", y); ("C", 0.0) ])

            let divReactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "C" ], 1) ] @ [ RxnS([ "B"; "C" ], [ "B" ], 1) ]

            let s = reactionSimulator divConcs divReactions 0.01
            let lastMap = Seq.item 250 s
            let a = Map.find "A" lastMap
            let b = Map.find "B" lastMap
            let c = Map.find "C" lastMap
            abs (c - (x / y))

        let z' = x' |> List.map (fun x -> (y' |> List.map (fun y -> divError x y)))

        let surface =
            Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.5, Contours = Contours.initXyz (Show = true))
        surface
    
    divErrorPlots |> Chart.show

if false then
    let sqrtErrorPlots =
        let x' = [ 0.0..1.0..100.0 ]

        let sqrtError x =
            let divConcs: State = Map([ ("A", x); ("B", 0) ])

            let divReactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "B" ], 1) ] @ [ RxnS([ "B"; "B"], [ ], 0.5) ]

            let s = reactionSimulator divConcs divReactions 0.01
            let lastMap = Seq.item 250 s
            let a = Map.find "A" lastMap
            let b = Map.find "B" lastMap
            abs (b - (sqrt x))

        let z' = x' |> List.map (fun x -> sqrtError x)
        Chart.Scatter(x = x',y=z', mode=StyleParam.Mode.Lines)
        
    sqrtErrorPlots |> Chart.show

if false then
    let LdErrorPlots =
        let x' = [ 0.0..1.0..60.0 ]

        let ldError x =
            let concs: State = Map([ ("A", x); ("B", 0) ])

            let reactions: RxnS list =
                [ RxnS([ "A" ], [ "A"; "B" ], 1) ] @ [ RxnS([ "B"; ], [ ], 1) ]

            let s = reactionSimulator concs reactions 0.01
            let lastMap = Seq.item 300 s
            let a = Map.find "A" lastMap
            let b = Map.find "B" lastMap
            abs (b - (x))
        let z' = x' |> List.map (fun x -> ldError x)

        Chart.Scatter(x = x',y=z', mode=StyleParam.Mode.Lines)
    LdErrorPlots |> Chart.show