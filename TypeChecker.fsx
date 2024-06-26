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
/// Don't allow conditional in the same step as CMP: ✔
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

type Flags(cmpFlag : bool, stepFlag : bool, cmpInStepFlag : bool, cndInStepFlag : bool) = 

    member this.Cmp = cmpFlag // has executed a CMP
    member this.Step  = stepFlag // has executed a Step
    member this.CmpInStep = cmpInStepFlag // has executed a CMP in this step

    member this.CndInStep = cndInStepFlag

type Env = Set<Species> * Flags
let TypeChecker root = 
    let isValidName (sp : Species) =
        if (Seq.head sp) = '_' then 
            false
        else true  

    let conchelper (conc : ConcS) (env : Env) =
        match conc, env with
        | _, (_,flags) 
            when flags.Step ->
            Some(sprintf "Cannot define concentrations after StepS"), env
        | (sp, _), (set,_) 
            when Set.contains sp set -> // verify species hasn't been initialized yet. 
            Some(sprintf "Preexisting species \'%s\' in env" sp), env
        | (sp, _), _
            when not (isValidName sp) ->
            Some(sprintf "Name is not valid for \'%s\'. Name must not begin with '_'" sp), env
        | (sp, nu), (set, flags) -> 
            None, ((Set.add sp set), flags)

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
        | CMP(a,b), (set, flags) ->
            if flags.CndInStep then
                Some(sprintf "CMP may not be executed in the same step as conditional"), env
            else
                let newFlags = Flags(true, flags.Step, true, flags.CndInStep) 
                binModHelper a b (set, newFlags)

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
        | IfGT(cmds), (_, flags) //(_, CMPflag(cflag), _, CMPinStepflag(cmpStepflag))
        | IfGE(cmds), (_, flags)
        | IfEQ(cmds), (_, flags)
        | IfLT(cmds), (_, flags)
        | IfLE(cmds), (_, flags) -> 
            if not flags.Cmp then 
                Some(sprintf "cannot run conditional-module before CMP-module"), env
            else if (flags.CmpInStep) then
                Some(sprintf "CMP may not be executed in the same step as conditional"), env
            else
                let stepOpt, newEnv = stephelper cmds env
                stepOpt, newEnv

    let rec rootListHelper rootList (env : Env) =
        match rootList, env with
        | [], (_, flags) when flags.CmpInStep -> 
            Some("cannot run CMP in last step.")
        | [], _ -> None
        | Conc(h)::tail, _ -> 
            let (r, newEnv) = conchelper h env
            lazyOptionSome r (rootListHelper tail newEnv)
        | Step(h)::tail, ((set, flags)) 
            ->
            let newFlags = Flags(flags.Cmp,true,false, false)
            let stepOpt, newEnv = stephelper h (set, newFlags)
            lazyOptionSome stepOpt (rootListHelper tail newEnv)

    match root with
    | CRN(rlist) -> rootListHelper rlist (Set.empty, Flags(false,false, false, false) : Env)


let extract p str =
    match run pCrn str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> CRN([])

if true then 
    let program1 = extract pCrn "crn = { conc[A,2],  conc[A,2], step[add[A,B,C]]} "
    printfn "%A" (TypeChecker program1)
if true then 
    let program2 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,C]]}"
    printfn "%A" (TypeChecker program2)
if true then 
    let program3 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,A]]}"
    printfn "%A" (TypeChecker program3)
if true then 
    let program4 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,C]]}"
    printfn "%A" (TypeChecker program4)
if true then 
    let program5 = extract pCrn "crn = {conc[A,2], conc[B,2],  conc[C,0], step[add[A,B,C]]}"
    printfn "%A" (TypeChecker program5)
if true then
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
    printfn "%A" (TypeChecker program6)


if true then
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
    printfn "%A" (TypeChecker program7)

if true then
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
    printfn "%A" (TypeChecker program8)

if true then
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