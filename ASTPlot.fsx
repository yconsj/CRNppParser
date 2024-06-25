// @author Simon Janum
// @author August Valentin
// @date 26/6/2024
#r "CRNpp\\Library\\net7.0\\CRNpp.dll"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: FParsec, 1.1.1"

open CRNpp.Parser
open DrawTrees.PlotTrees
open DrawTrees.Trees


let drawAST (root: Crn) =
    let conchelper (conc: ConcS) =
        match conc with
        | (sp, nu) ->
            [ Node("Species", [ Node(sprintf "\'%s\'" sp, []) ])
              Node("Number", [ Node(string nu, []) ]) ]

    let triModHelper (a: Species) (b: Species) (c: Species) =
        [ Node("Species", [ Node(sprintf "\'%s\'" a, []) ])
          Node("Species", [ Node(sprintf "\'%s\'" b, []) ])
          Node("Species", [ Node(sprintf "\'%s\'" c, []) ]) ]

    let binModHelper (a: Species) (b: Species) =
        [ Node("Species", [ Node(sprintf "\'%s\'" a, []) ])
          Node("Species", [ Node(sprintf "\'%s\'" b, []) ]) ]

    let moduleHelper md =
        match md with
        | ADD(a, b, c) -> Node("add", (triModHelper a b c))
        | SUB(a, b, c) -> Node("sub", (triModHelper a b c))
        | MUL(a, b, c) -> Node("mul", (triModHelper a b c))
        | DIV(a, b, c) -> Node("div", (triModHelper a b c))
        | LD(a, b) -> Node("ld", (binModHelper a b))
        | SQRT(a, b) -> Node("sqrt", (binModHelper a b))
        | CMP(a, b) -> Node("cmp", (binModHelper a b))

    let rec stephelper (step: CommandS list) =
        match step with
        | [] -> []
        | Module(h) :: tail -> Node("Module", [ moduleHelper h ]) :: (stephelper tail)
        | Conditional(h) :: tail -> Node("Conditional", [ conditionalhelper h ]) :: (stephelper tail)

    and conditionalhelper cond =
        match cond with
        | IfGT(cmds) -> Node("IfGT", stephelper cmds)
        | IfGE(cmds) -> Node("IfGE", stephelper cmds)
        | IfEQ(cmds) -> Node("IfEQ", stephelper cmds)
        | IfLT(cmds) -> Node("IfLT", stephelper cmds)
        | IfLE(cmds) -> Node("IfLE", stephelper cmds)

    let rec rootListHelper rootList : Tree<Species> list =
        match rootList with
        | [] -> []
        | Conc(h) :: tail -> Node("Concs", (conchelper h)) :: (rootListHelper tail)
        | Step(h) :: tail -> Node("Step", stephelper h) :: (rootListHelper tail)

    match root with
    | CRN(rlist) ->
        let rt = Node("Crn", rootListHelper rlist)
        let rtDesign = design rt
        visualizeTree rtDesign false

let testProgram =
    "
            crn = {
            conc[c,2 ], conc[ cInitial ,2 ],
            conc[one ,1], conc[zero ,0],
            step[
            sub[c,one,cnext ],
            cmp[c,zero]
            ],
            step[
            ifGT[ ld [cnext ,c] ],
             ifLE[ ld [ cInitial ,c] ]
             ] }
    "

drawAST (parseCrn testProgram)
