#r "CRNppInterpreter/Library/net7.0/CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"

open Plotly.NET
open Plotly.NET.LayoutObjects

open Interpreter.Parser
open Interpreter.Interpreter
open Interpreter.Plotter



// 14. Compile stp : Step, that is, a list of commands, to chemical reaction networks. (See
// Table 1).

let table1 m =
    match m with
    | LD(A, B) -> [ Rxn([ A ], [ A; B ], 1); Rxn([ B ], [], 1) ]
    | ADD(A, B, C) -> [ Rxn([ A ], [ A; C ], 1); Rxn([ B ], [ B; C ], 1); Rxn([ C ], [], 1) ]
    | SUB(A, B, C) -> [] //TODO
    | MUL(A, B, C) -> [ Rxn([ A; B ], [ A; B; C ], 1); Rxn([ C ], [], 1) ]
    | DIV(A, B, C) -> [ Rxn([ A ], [ A; C ], 1); Rxn([ B; C ], [ B ], 1) ]
    | SQRT(A, B) -> [ Rxn([ A ], [ A; B ], 1); Rxn([ B; B ], [], 0.5) ]
    | CMP(A, B) -> [] // TODO

let compile stp =
    match stp with
    | Module(x) :: t -> (table1 x) @ t
