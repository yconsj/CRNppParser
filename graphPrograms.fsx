#r "CRNppInterpreter/Library/net7.0/CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"

open Plotly.NET
open Plotly.NET.LayoutObjects

open Interpreter.Parser
open Interpreter.Interpreter
open Interpreter.Plotter

let program1 =
    "
    crn = {
    conc[c,3 ], conc[ cInitial ,3 ],
    conc[one ,1], conc[zero ,0],
    step[
    sub[c,one,cnext ],
    cmp[c,zero]
    ],
    step[
    ifGT[ ld [cnext ,c] ],
    ifLE[ ld [ cInitial ,c] ]
    ]
     }  
    
    "

let parsedProgram1 = parseCrn program1
printf "%A\n" parsedProgram1
let interpretedProgram1 = interpretProgram parsedProgram1


printf "%A \n" interpretedProgram1
(plotter interpretedProgram1 30) |> Chart.show
