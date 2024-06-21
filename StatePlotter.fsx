#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: Plotly.NET, 4.0.0"
//#r "nuget: FParsec, 1.1.1"
//open FParsec
open Interpreter.Parser
open Plotly.NET
open Plotly.NET.LayoutObjects

type State = Map<Species, Number>


let example = seq{
    Map([("a",2.0); 
        ("b",0.0);
        ("c",0.0);
        ("d",0.0);
    ]);
    Map([("a",2.0); 
        ("b",2.0);
        ("c",0.0);
        ("d",0.0);
    ]);
    Map([("a",2.0); 
        ("b",2.0);
        ("c",4.0);
        ("d",0.0);
    ]);
    Map([("a",0.0); 
        ("b",2.0);
        ("c",4.0);
        ("d",0.0);
    ]);
    Map([("a",0.0); 
        ("b",2.0);
        ("c",4.0);
        ("d",6.0);
    ]);
}

let plotter (simData : seq<State>) nSteps =

    let rec helperFunc (species : Species) (data : State seq) =
        match Seq.toList data with
        | [] -> seq []
        | h::tail -> 
        let sq1 = seq {(Map.find species h)}
        Seq.append sq1 (helperFunc species tail)
    let rec helperFunc2 keys xdata ydata =
        match keys with
        | h::tail -> [Chart.Scatter(xdata, (helperFunc h ydata), mode=StyleParam.Mode.Lines, Name=h)] @ (helperFunc2 tail xdata ydata)    
        | [] -> []
    let nData = Seq.take nSteps simData 
    let x = [0 .. 1 .. nSteps]
    let keys = List.ofSeq (Seq.head nData).Keys
    let charts = helperFunc2 (keys) x nData
    charts |> Chart.combine

(plotter example 5) |> Chart.show;