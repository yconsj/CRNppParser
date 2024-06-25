
#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open Interpreter.Parser
open Interpreter.Simulator
open Plotly.NET
open Plotly.NET.TraceObjects



type State = Map<Species, Number>

let AddErrorPlots =
    let x' = [0.0 .. 1.0 .. 30.0]
    let y' = x'


    // row wise (length x)
    // column (length y)
    //let z' =  [[ 1.; 1.]; 
    //    [ 1.; 2. ]] ; 
    let addError x y =
        let addConcs : State = Map([("A",x);("B",y);("C",0.0)])
        let addReactions : RxnS list = [RxnS(["A"],["A";"C"],1);
            RxnS(["B"],["B";"C"],1);
            RxnS(["C"],[],1)]
       
        let s = reactionSimulator addConcs addReactions 1
        let lastMap = Seq.last (Seq.take 100 s)
        let c = Map.find "C" lastMap
        
        abs (c - (x + y))

    let z' = 
        x' |> List.map (fun x -> 
            ( y' |> List.map (fun y -> addError x y ) )
        )
    // printfn "%A" z'
    let surface =
        Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.5, Contours = Contours.initXyz (Show = true))
    surface

// AddErrorPlots |> Chart.show 


let SubErrorPlots =
    let x' = [0.0 .. 1.0 .. 60.0]
    let y' = x'

    // row wise (length x)
    // column (length y)
    //let z' =  [[ 1.; 1.]; 
    //    [ 1.; 2. ]] ; 
    let subError x y =
        let subConcs : State = Map([("A",x);("B",y);("C",0.0);("H",0.0)])
        let subReactions : RxnS list = [RxnS(["A"],["A";"C"],1);
            RxnS(["B"],["B";"H"],1);
            RxnS(["C"],[],1);
            RxnS(["C";"H"],[],1)]
        let s = reactionSimulator subConcs subReactions 0.01
        let lastMap = Seq.last (Seq.take 250 s)
        let c = Map.find "C" lastMap
        
        if x > y then
            abs (c - (x - y))
        else
            abs(c)

    let z' = 
        x' |> List.map (fun x -> 
            ( y' |> List.map (fun y -> subError x y ) )
        )
    printfn "%A" z'
    let surface =
        Chart.Surface(zData = z', X = x', Y = y', Opacity = 0.5, Contours = Contours.initXyz (Show = true))
    surface

SubErrorPlots |> Chart.show 