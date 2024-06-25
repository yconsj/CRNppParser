//#r "CRNppInterpreter\\Parser.fs"
#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
open Interpreter.Parser
open FParsec
open System

type CMPflag = CMPflag of bool
type CMPinStepflag = CMPinStepflag of bool
type STEPflag = STEPflag of bool

/// TypeChecker:
/// Check the explicit restrictions: ✔
/// Check CMP is executed in a step prior to ConditionalS: ✔
/// Dont allow CMP in last step: ✔
/// dont allow names starting with underscore (protected names): ✔
/// dont allow empty LHS on Rxn: ✔
/// dont allow Conc on the same species multiple times: ✔
/// a ConcS cannot follow after StepS: ✔ 
/// 
/// Chosen NOT to enforce:
/// (Not true:) only non-conflicting commands in Steps
/// (Not true:) only initialized (conc) species allowed in (source-variable of?) modules
///  protected symbols (such as XgtY, XltY)
/// 


let lazyOptionSome opt1 opt2 =
    if Option.isSome opt1 then 
        opt1 
    else
        opt2

type Env = Map<Species, Number> * CMPflag * STEPflag * CMPinStepflag // initialized variables, has executed a CMP, has executed a Step

let TypeChecker root = 
    let isValidName (sp : Species) =
        if (Seq.head sp) = '_' then 
            false
        else true  

    let conchelper (conc : ConcS) (env : Env) =
        match conc, env with
        | _, (_, _, STEPflag(sflag),_) 
            when sflag ->
            Some(sprintf "Cannot define concentrations after StepS"), env
        | (sp, _), (map, _, _, _) 
            when Map.containsKey sp map -> // verify species hasn't been initialized yet. 
            Some(sprintf "Preexisting species \'%s\' in env" sp), env
        | (sp, _), _
            when not (isValidName sp) ->
            Some(sprintf "Name is not valid for \'%s\'. Name must not begin with '_'" sp), env
        | (sp, nu), (map, cflag, sflag, cthisflag) -> 
            None, ((Map.add sp nu map), cflag, sflag, cthisflag)

    let IsSameSpecies sA sB =
        if (sA = sB) then
            Some(sprintf "Conflict in Module with Species: \'%s\'" sA)
        else
            None
            
    let triModHelper (a : Species) (b : Species) (c : Species) (env : Env) =
        (lazyOptionSome (IsSameSpecies a c) (IsSameSpecies b c)), env
    
    let binModHelper (a : Species) (b : Species) (env : Env) =
        IsSameSpecies a b, env

    let moduleHelper md (env : Env) =
        match md, env with
        | ADD(a,b,c), _
        | SUB(a,b,c), _
        | MUL(a,b,c), _
        | DIV(a,b,c), _ -> triModHelper a b c env //...
        | LD(a,b), _
        | SQRT(a,b), _ -> binModHelper a b env
        | CMP(a,b), (map, _, sflag, cthisflag) -> 
            binModHelper a b (map, CMPflag(true), sflag, CMPinStepflag(true))

    let rxnhelper (rxn : RxnS) =
        match rxn with
        | (lhs, rhs, rate) when List.isEmpty lhs->
            Some(sprintf "Rxn has empty left-hand side: \'%A -> %A : %f\'" lhs rhs rate)
        | _ -> None

    let rec stephelper (step : CommandS list ) (env : Env) =
        match (step) with
        | [] -> None, env
        | Rxn(h)::tail -> 
            let rxnOpt = (rxnhelper h)
            let stepOpt, newEnv = (stephelper tail env)
            (lazyOptionSome (rxnOpt) (stepOpt)), newEnv
        | Module(h)::tail -> 
            let modOpt, newEnv = (moduleHelper h env)
            let stepOpt, newEnv = (stephelper tail newEnv)
            lazyOptionSome (modOpt) (stepOpt), newEnv
        | Conditional(h)::tail ->
            let condOpt, newEnv = conditionalhelper h env
            let stepOpt, newEnv = (stephelper tail newEnv)
            lazyOptionSome (condOpt)(stepOpt), newEnv
    and conditionalhelper cond env =
        match cond, env with
        | IfGT(cmds), (_, CMPflag(cflag), _, _)
        | IfGE(cmds), (_, CMPflag(cflag), _, _)
        | IfEQ(cmds), (_, CMPflag(cflag), _, _)
        | IfLT(cmds), (_, CMPflag(cflag), _, _)
        | IfLE(cmds), (_, CMPflag(cflag), _, _) -> 
            if not cflag then 
                Some(sprintf "cannot run conditional-module before CMP-module"), env
            else
                let stepOpt, newEnv = stephelper cmds env
                stepOpt, newEnv

    let rec rootListHelper rootList (env : Env) =
        match rootList, env with
        | [], (_, _, _, CMPinStepflag(cmpstepflag)) when cmpstepflag -> 
            Some("cannot run CMP in last step.")
        | [], _ -> None
        | Conc(h)::tail, _ -> 
            let (r, newEnv) = conchelper h env
            lazyOptionSome r (rootListHelper tail newEnv)
        | Step(h)::tail, ((map, cflag, sflag, cmpstepflag)) 
            ->
            let stepOpt, newEnv = stephelper h (((map, cflag, sflag, CMPinStepflag(false))))
            lazyOptionSome stepOpt (rootListHelper tail newEnv)

    match root with
    | CRN(rlist) -> rootListHelper rlist ((Map.empty, CMPflag(false), STEPflag(false), CMPinStepflag(false)) : Env)


let extract p str =
    match run pCrn str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> CRN([])

let program1 = extract pCrn "crn = { conc[A,2],  conc[A,2], step[add[A,B,C]]} "
if false then 
    printfn "%A" (TypeChecker program1)
let program2 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,C]]}"
if false then 
    printfn "%A" (TypeChecker program2)
let program3 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,A]]}"
if false then 
    printfn "%A" (TypeChecker program3)
let program4 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,C]]}"
if false then 
    printfn "%A" (TypeChecker program4)
let program5 = extract pCrn "crn = {conc[A,2], conc[B,2],  conc[C,0], step[add[A,B,C]]}"
if false then 
    printfn "%A" (TypeChecker program5)

let program6 = extract pCrn "
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
if false then 
    printfn "%A" (TypeChecker program6)

let program7 = extract pCrn "
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
    printfn "%A" (TypeChecker program7)

let program8 = extract pCrn  "
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

if false then 
    printfn "%A" (TypeChecker program8)

let program9 = extract pCrn" crn={
        conc[ f ,1], conc[one ,1], conc[ i , 5 ],
        step[
            cmp[i,one ]
        ],
        step[
            ifGT[
            cmp[i,one]
            ]
        ],
        step[
            cmp[i,one ]
        ]
    }
    "
printfn "%A" (TypeChecker (program9))