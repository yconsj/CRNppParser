#r "CRNppInterpreter/Library/net7.0/CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"

open Plotly.NET
open Plotly.NET.LayoutObjects

open Interpreter.Parser
open Interpreter.Plotter

#load "ReactionSimulator.fsx"
open Sim


// 14. Compile stp : Step, that is, a list of commands, to chemical reaction networks. (See
// Table 1).

let table1 m n catalystL =
    printfn "%A \n" catalystL
    let clockSpecies = "_X" + n.ToString()


    match m with
    | LD(A, B) ->
        [ RxnS(catalystL @ [ clockSpecies; A ], catalystL @ [ clockSpecies; A; B ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; B ], catalystL @ [ clockSpecies ], 1) ]
    | ADD(A, B, C) ->
        [ RxnS(catalystL @ [ clockSpecies; A ], catalystL @ [ clockSpecies; A; C ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; B ], catalystL @ [ clockSpecies; B; C ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; C ], catalystL @ [ clockSpecies ], 1) ]
    | SUB(A, B, C) ->
        let H = "_" + A + B + C

        [ RxnS(catalystL @ [ clockSpecies; A ], catalystL @ [ clockSpecies; A; C ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; B ], catalystL @ [ clockSpecies; B; H ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; C ], catalystL @ [ clockSpecies ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; C; H ], catalystL @ [ clockSpecies ], 1) ]
    | MUL(A, B, C) ->
        [ RxnS(catalystL @ [ clockSpecies; A; B ], catalystL @ [ clockSpecies; A; B; C ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; C ], catalystL @ [ clockSpecies ], 1) ]
    | DIV(A, B, C) ->
        [ RxnS(catalystL @ [ clockSpecies; A ], catalystL @ [ clockSpecies; A; C ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; B; C ], [ clockSpecies; B ], 1) ]
    | SQRT(A, B) ->
        [ RxnS(catalystL @ [ clockSpecies; A ], catalystL @ [ clockSpecies; A; B ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; B; B ], catalystL @ [ clockSpecies ], 0.5) ]
    | CMP(X, Y) ->
        // AgtB + B
        let XgtY = "_XgtY"
        let XltY = "_XltY"
        let YgtX = "_YgtX"
        let YltX = "_YltX"
        let Bx = "_Bx"
        let By = "_By"
        let nextClockSpecies = "_X" + (n + 3).ToString()
        //TODO ADD CLOCK SPECIES

        [ RxnS(catalystL @ [ clockSpecies; XgtY; Y ], catalystL @ [ clockSpecies; XltY; Y ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; XltY; X ], catalystL @ [ clockSpecies; XgtY; X ], 1) ]

        @ [ RxnS(catalystL @ [ clockSpecies; YgtX; X ], catalystL @ [ clockSpecies; YltX; X ], 1) ]
        @ [ RxnS(catalystL @ [ clockSpecies; YltX; Y ], catalystL @ [ clockSpecies; YgtX; Y ], 1) ]

        // CRN8
        // move to next step

        @ [ RxnS(catalystL @ [ nextClockSpecies; XgtY; XltY ], catalystL @ [ nextClockSpecies; XltY; Bx ], 1) ]
        @ [ RxnS(catalystL @ [ nextClockSpecies; Bx; XltY ], catalystL @ [ nextClockSpecies; XltY; XltY ], 1) ]
        @ [ RxnS(catalystL @ [ nextClockSpecies; XltY; XgtY ], catalystL @ [ nextClockSpecies; XgtY; Bx ], 1) ]
        @ [ RxnS(catalystL @ [ nextClockSpecies; Bx; XgtY ], catalystL @ [ nextClockSpecies; XgtY; XgtY ], 1) ]

        @ [ RxnS(catalystL @ [ nextClockSpecies; YgtX; YltX ], catalystL @ [ nextClockSpecies; YltX; By ], 1) ]
        @ [ RxnS(catalystL @ [ nextClockSpecies; By; YltX ], catalystL @ [ nextClockSpecies; YltX; YltX ], 1) ]
        @ [ RxnS(catalystL @ [ nextClockSpecies; YltX; YgtX ], catalystL @ [ nextClockSpecies; YgtX; By ], 1) ]
        @ [ RxnS(catalystL @ [ nextClockSpecies; By; YgtX ], catalystL @ [ nextClockSpecies; YgtX; YgtX ], 1) ]

let rec compileStep stp clockSpecies catalysts =


    match stp with
    | [] -> []
    | Module(x) :: t -> (table1 x clockSpecies catalysts) @ compileStep t clockSpecies catalysts
    | Conditional(IfGT(x)) :: t ->
        (compileStep x clockSpecies ("_YltX" :: "_XgtY" :: catalysts))
        @ compileStep t clockSpecies catalysts
    | Conditional(IfGE(x)) :: t -> (compileStep x clockSpecies ("?" :: catalysts)) @ compileStep t clockSpecies []
    | Conditional(IfEQ(x)) :: t ->
        (compileStep x clockSpecies ("_YgtX" :: "_XgtY" :: catalysts))
        @ compileStep t clockSpecies catalysts
    | Conditional(IfLT(x)) :: t ->
        (compileStep x clockSpecies ("_YgtX" :: "_XltY" :: catalysts))
        @ compileStep t clockSpecies catalysts
    | Conditional(IfLE(x)) :: t ->
        (compileStep x clockSpecies ("_XltY" :: "_YgtX" :: "_XgtY" :: catalysts))
        @ compileStep t clockSpecies catalysts
    | _ -> []

let intilizeClockSpecies n =
    let rec initializeClockSpecies' i n =
        match i with
        | i when n = i -> [ RxnS([ "_X" + n.ToString(); "_X1" ], [ "_X1"; "_X1" ], 1) ]
        | _ ->
            [ RxnS(
                  [ "_X" + i.ToString(); "_X" + (i + 1).ToString() ],
                  [ "_X" + (i + 1).ToString(); "_X" + (i + 1).ToString() ],
                  1
              ) ]
            @ initializeClockSpecies' (i + 1) n

    initializeClockSpecies' 1 n



let rec intilizeClockSpeciesConc n m =
    match n with
    | 0 -> m
    | n -> intilizeClockSpeciesConc (n - 1) (Map.add ("_X" + n.ToString()) 0.0 m)


let rec nSteps rl =
    match rl with
    | [] -> 0
    | Conc(x) :: t -> nSteps t
    | Step(x) :: t -> 1 + nSteps t

let compile stps =
    let nSteps = nSteps stps

    let rec compileSteps stps' n =
        match stps' with
        | [] -> []
        | Step(stp) :: t -> compileStep stp (n) [] @ compileSteps t (n + 3)
        | Conc(x) :: t -> compileSteps t (n)


    let reactions = intilizeClockSpecies (3 * nSteps) @ (compileSteps stps 3)

    let x =
        List.fold
            (fun acc (xl, yl, n) -> acc @ (List.map (fun x -> (x, 0.0)) xl) @ (List.map (fun x -> (x, 0)) yl))
            []
            reactions

    let initialConcs = Map.ofList x

    let rec compileInitialConcs rl state =
        match rl with
        | [] -> state
        | Conc(x, y) :: t -> compileInitialConcs t (Map.add x y state)
        | Step(x) :: t -> compileInitialConcs t state

    let initialConcs = compileInitialConcs stps initialConcs
    // add clocks
    let rec clockConcs n acc =
        match n with
        | 1 -> Map.add ("_X1") 2.0 acc
        | n -> clockConcs (n - 1) (Map.add ("_X" + n.ToString()) 0.01 acc)

    let XgtY = "_XgtY"
    let XltY = "_XltY"
    let YgtX = "_YgtX"
    let YltX = "_YltX"

    let initialConcs =
        Map.add
            XgtY
            0.5
            (Map.add XltY 0.5 (Map.add YgtX 0.5 (Map.add YltX 0.5 ((clockConcs (3 * nSteps) initialConcs)))))

    printf "%A" initialConcs
    printf "%A" reactions
    (reactions, initialConcs)

let program1 =
    "
    crn = {
    conc[e, 1], conc[element, 1],
    conc[ divisor , 1], conc[one, 1],
    conc[ divisorMultiplier , 1],
    step[
    div [element, divisor , elementNext],
    add[ divisor , one, divisorNext ],
    add[e, elementNext, eNext]
    ],
    step[
    ld [elementNext, element ],
    ld [ divisorNext , divisor ],
    ld [eNext, e]
     ]
    }
    
    "

let program2 =
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

let program3 =
    "

crn={
    conc[ f ,1], conc[one ,1], conc[ i , 5 ],
    step[
    cmp[i,one ],
    mul[f , i , fnext ],
    sub[ i ,one, inext ]
    ],
    step[
    ifGT[
    ld [ inext , i ],
    ld [ fnext , f ]
    ]
    ]
    }
"

let parsedProgram = parseCrn program3

let (CRN x) = parsedProgram
let (reactions, initial) = compile x

reactionSimulatorPlot initial reactions 0.5 200
