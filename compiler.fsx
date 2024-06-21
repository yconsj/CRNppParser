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

let table1 m clockSpecies =
    match m with
    | LD(A, B) -> [ Rxn([clockSpecies; A ], [ clockSpecies; A; B ], 1)]
                    @[Rxn([ clockSpecies; B ], [clockSpecies], 1) ]
    | ADD(A, B, C) -> [ Rxn([ clockSpecies;A ], [ clockSpecies; A; C ], 1)]
                      @[ Rxn([clockSpecies;  B ], [ clockSpecies; B; C ], 1)]
                      @[ Rxn([ clockSpecies; C ], [clockSpecies], 1) ]
    | SUB(A, B, C) -> 
        let H = "_" + A + B + C
        [ Rxn( [clockSpecies; A], [clockSpecies; A;C],1 ) ]
        @[ Rxn( [clockSpecies; B], [clockSpecies; B; H], 1 ) ]
        @[ Rxn([clockSpecies; C],[clockSpecies],1) ]
        @[ Rxn([clockSpecies; C; H], [clockSpecies], 1)  ] //TODO
    | MUL(A, B, C) -> [ Rxn([ clockSpecies; A; B ], [clockSpecies;  A; B; C ], 1)]
                        @[ Rxn([clockSpecies;  C ], [clockSpecies], 1) ]
    | DIV(A, B, C) -> [ Rxn([ clockSpecies; A ], [clockSpecies;  A; C ], 1)]
                        @[ Rxn([clockSpecies;  B; C ], [ clockSpecies; B ], 1) ]
    | SQRT(A, B) -> [ Rxn([ clockSpecies; A ], [clockSpecies;  A; B ], 1)]
                     @[ Rxn([clockSpecies;  B; B ], [clockSpecies], 0.5) ]
    | CMP(X, Y) -> 
        // AgtB + B
        let XgtY = "XgtY"
        let XltY = "XltY"
        let B = "_B"
        [Rxn([XgtY;Y], [XltY;Y], 1)]
        @[Rxn([XltY;X], [XgtY;X],1)]

        // CRN8
        @[Rxn([XgtY;XltY], [XltY;B],1)]
        @[Rxn([B;XltY], [XltY;XltY],1)]
        @[Rxn ([XltY;XgtY], [XgtY;B],1)]
        @[Rxn ([B;XgtY], [XgtY;XgtY],1)]

let rec compileStep stp clockSpecies =
    match stp with
    | [] -> []
    | Module(x) :: t -> (table1 x clockSpecies) @ compileStep t clockSpecies

let intilizeClockSpecies n =
    let rec initializeClockSpecies' i n =
        match i with
        | i  when n = i-> [ Rxn(["_X" + n.ToString() ; "_X1"], ["_X1";"_X1"],1)]
        | _ -> [ Rxn(["_X" + i.ToString(); "_X"+(i+1).ToString()], ["_X"+(i+1).ToString(); "_X"+(i+1).ToString()],1)] 
                @ initializeClockSpecies' (i+1) n
    initializeClockSpecies' 1 n 
let compile stps =
    let nSteps = List.length stps
    let clockSpecies = intilizeClockSpecies (nSteps*3)
    
    let rec compileSteps stps' n =
        match stps' with
        | [] -> []
        | stp::t -> compileStep stp ("_X" + n.ToString()) @ compileSteps t (n+3)

    compileSteps stps
