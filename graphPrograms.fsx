// @author Simon Janum
// @author August Valentin
// @date 26/6/2024
#r "CRNpp/Library/net7.0/CRNpp.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open Plotly.NET
open Plotly.NET.LayoutObjects

open CRNpp.Parser
open CRNpp.Interpreter
open CRNpp.Plotter
open CRNpp.Simulator
open CRNpp.Compiler

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

if false then
    let interpretedProgram1 = interpretProgram program1
    printf "%A \n" interpretedProgram1
    simulationPlot interpretedProgram1 30



// Exercise 1.6:
// Some programs executions may converge to a steady state (e.g. that for GCD) while
// others will not (e.g. Discrete counter). Construct a visualization component so that
// figures like Fig. 3(b) and Fig. 6(b) can be shown
let programGCD =
    "
    crn = { 
        conc[a,32 ],
        conc[b,12 ],
        step [
            ld [a, atmp],
            ld [b, btmp],
            cmp[a,b]
        ],
        step [
            ifGT[ sub[atmp,btmp,a] ],
            ifLT [ sub[btmp,atmp,b] ]
        ]
    };
    "

if false then
    let interpretedProgramGCD = interpretProgram programGCD
    simulationPlot interpretedProgramGCD 10


let programDiscreteCounter =
    " 
    crn = {
        conc[c,3 ], conc[ cInitial ,3],
        conc[one ,1], conc[zero ,0],
        step [
            sub[c,one,cnext ],
            cmp[c,zero ]
        ],
        step [
            ifGT[  ld [cnext,c ] ],
            ifLE [ ld [ cInitial ,c ] ]
            ]
    }
     "

if false then
    let interpretedProgramDiscreteCounter = interpretProgram programDiscreteCounter

    simulationPlot interpretedProgramDiscreteCounter 20

if false then
    let fig1MulConcs: State = Map([ ("A", 6.0); ("B", 2.0); ("C", 0.0); ("D", 0.0) ])

    let fig1MulReaction: RxnS list =
        [ RxnS([ "A"; "B" ], [ "A"; "B"; "C" ], 1); RxnS([ "C" ], [], 1) ]

    reactionSimulatorPlot fig1MulConcs fig1MulReaction 0.1 15

if false then
    let fig4OscConcs: State =
        Map([ ("X_1", 0.9999999999); ("X_2", 0.0000000002); ("X_3", 0.9999999999) ])

    let fig4OscReaction: RxnS list =
        [ RxnS([ "X_1"; "X_2" ], [ "X_2"; "X_2" ], 1.0)
          RxnS([ "X_2"; "X_3" ], [ "X_3"; "X_3" ], 1.0)
          RxnS([ "X_3"; "X_1" ], [ "X_1"; "X_1" ], 1.0) ]

    reactionSimulatorPlot fig4OscConcs fig4OscReaction 0.1 100

