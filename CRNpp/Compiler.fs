namespace CRNpp

module Compiler =
    open Parser
    open TypeChecker

    let table1 m n catalystL =
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
            let epsilon = "_epsilon"
            let epsX = "_epsX"
            let epsY = "_epsY"
            let nextClockSpecies = "_X" + (n + 1).ToString()



            // add epsilon X
            [ RxnS(catalystL @ [ clockSpecies; X ], catalystL @ [ clockSpecies; X; epsX ], 1) ]
            @ [ RxnS(catalystL @ [ clockSpecies; epsilon ], catalystL @ [ clockSpecies; epsilon; epsX ], 1) ]
            @ [ RxnS(catalystL @ [ clockSpecies; epsX ], catalystL @ [ clockSpecies ], 1) ]


            // add epsilon Y
            @ [ RxnS(catalystL @ [ clockSpecies; Y ], catalystL @ [ clockSpecies; Y; epsY ], 1) ]
            @ [ RxnS(catalystL @ [ clockSpecies; epsilon ], catalystL @ [ clockSpecies; epsilon; epsY ], 1) ]
            @ [ RxnS(catalystL @ [ clockSpecies; epsY ], catalystL @ [ clockSpecies ], 1) ]

            // cmp
            @ [ RxnS(catalystL @ [ clockSpecies; XgtY; Y ], catalystL @ [ clockSpecies; XltY; Y ], 1) ]
            @ [ RxnS(catalystL @ [ clockSpecies; XltY; epsX ], catalystL @ [ clockSpecies; XgtY; epsX ], 1) ]

            @ [ RxnS(catalystL @ [ clockSpecies; YgtX; X ], catalystL @ [ clockSpecies; YltX; X ], 1) ]
            @ [ RxnS(catalystL @ [ clockSpecies; YltX; epsY ], catalystL @ [ clockSpecies; YgtX; epsY ], 1) ]

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
        | Conditional(IfGE(x)) :: t ->
            (compileStep x clockSpecies ("_XgtY" :: catalysts))
            @ compileStep t clockSpecies catalysts
        | Conditional(IfEQ(x)) :: t ->
            (compileStep x clockSpecies ("_YgtX" :: "_XgtY" :: catalysts))
            @ compileStep t clockSpecies catalysts
        | Conditional(IfLT(x)) :: t ->
            (compileStep x clockSpecies ("_YgtX" :: "_XltY" :: catalysts))
            @ compileStep t clockSpecies catalysts
        | Conditional(IfLE(x)) :: t ->
            (compileStep x clockSpecies ("_YgtX" :: catalysts))
            @ compileStep t clockSpecies catalysts
        | Rxn(x, y, v) :: t ->
            let clockSpeciesName = "_X" + clockSpecies.ToString()

            [ RxnS([ clockSpeciesName ] @ catalysts @ x, [ clockSpeciesName ] @ catalysts @ y, v) ]
            @ compileStep t clockSpecies catalysts

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

    let compile crn =
        let (CRN rl) = parseCrn crn

        match TypeChecker(CRN rl) with
        | Some(x) -> failwith x
        | None ->
            let nSteps = nSteps rl

            let rec compileSteps stps' n =
                match stps' with
                | [] -> []
                | Step(stp) :: t -> compileStep stp (n) [] @ compileSteps t (n + 3)
                | Conc(x) :: t -> compileSteps t (n) // skip conc


            let reactions = intilizeClockSpecies (3 * nSteps) @ (compileSteps rl 3)

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

            let initialConcs = compileInitialConcs rl initialConcs
            // add clocks
            let rec clockConcs n v acc =

                match n with
                | 1 -> acc
                | n -> clockConcs (n - 1) v (Map.add ("_X" + n.ToString()) v acc)

            let v = 0.000000001
            let startC = ((2.0 - (v * (float (nSteps * 3)))) / 2.0) + v
            let initialConcs = (clockConcs (3 * nSteps) v initialConcs)

            let initialConcs =
                Map.add ("_X" + (nSteps * 3).ToString()) startC (Map.add "_X1" startC initialConcs)

            let XgtY = "_XgtY"
            let XltY = "_XltY"
            let YgtX = "_YgtX"
            let YltX = "_YltX"

            let initialConcs =
                Map.add XgtY 0.5 (Map.add XltY 0.5 (Map.add YgtX 0.5 (Map.add YltX 0.5 (initialConcs))))

            let epsilon = "_epsilon"
            let initialConcs = Map.add epsilon 0.5 initialConcs
            let initialConcs = Map.add "_epsY" 0.0 (Map.add "_epsX" 0.0 initialConcs)
            (reactions, initialConcs)
