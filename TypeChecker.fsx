//#r "CRNppInterpreter\\Parser.fs"
#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
open Interpreter.Parser
open FParsec

/// TypeChecker:
/// Check the explicit restrictions
/// Check CMP is executed in a step prior to ConditionalS
/// (Not true:) only non-conflicting commands in Steps
/// (Not true:) only initialized (conc) species allowed in (source-variable of?) modules
/// a conc cannot follow after StepS(?)
/// protected symbols (such as XgtY, XltY), or just not allow names starting with e.g. underscore
/// dont allow Conc on the same species multiple times.
/// dont allow empty LHS on Rxn

let lazyOptionSome opt1 opt2 =
    if Option.isSome opt1 then 
        opt1 
    else
        opt2

type Env = Map<Species, Number> * bool // initialized variables, has run a CMP

let TypeChecker root = 
    let conchelper (conc : ConcS) (env : Env) =
        match conc, env with
        | (sp, nu), (map, b) -> 
            if (Map.containsKey sp map) then
                Some(sprintf "Preexisting species \'%s\' in env" sp), env
            else
                None, ((Map.add sp nu map), b)
    let IsNotInitializedVariable vars env =
        match env with (map, _) ->
        let uninitialized = Seq.tryFind (fun x -> not (Map.containsKey x map)) vars
        if (Option.isSome uninitialized) then
            Some(sprintf "Reference to uninitialized Species: \'%s\'" (Option.get uninitialized))
        else
            None

    let IsSameSpecies sA sB =
        if (sA = sB) then
            Some(sprintf "Conflict in Module with Species: \'%s\'" sA)
        else
            None
            
    let triModHelper (a : Species) (b : Species) (c : Species) env =
        let vars = seq {a;b;c}
        let uninitialized = (IsNotInitializedVariable vars env)
        let AeqC = IsSameSpecies a c
        lazyOptionSome (lazyOptionSome uninitialized AeqC) (IsSameSpecies b c)
    
    let binModHelper (a : Species) (b : Species) env =
        let vars = seq {a;b}
        let uninitialized = (IsNotInitializedVariable vars env)
        let Aeqb = IsSameSpecies a b
        lazyOptionSome uninitialized Aeqb

    let moduleHelper md env =
        match md with
        | ADD(a,b,c)
        | SUB(a,b,c)
        | MUL(a,b,c)
        | DIV(a,b,c) -> triModHelper a b c env //...
        | LD(a,b)
        | SQRT(a,b)
        | CMP(a,b) -> binModHelper a b env

    let rec stephelper (step : CommandS list ) env =
        match (step) with
        | [] -> None
        | Rxn(h)::tail -> stephelper tail env
        | Module(h)::tail -> lazyOptionSome (moduleHelper h env) (stephelper tail env)
        | Conditional(h)::tail ->
            let r = conditionalhelper h env
            lazyOptionSome r (stephelper tail env)
    and conditionalhelper cond env =
        match cond with
        | IfGT(cmds) 
        | IfGE(cmds)
        | IfEQ(cmds)
        | IfLT(cmds)
        | IfLE(cmds) -> stephelper cmds env

    let rec rootListHelper rootList env =
        match rootList with
        | []-> None
        | Conc(h)::tail -> 
            let (r,m) = conchelper h env
            lazyOptionSome r (rootListHelper tail env)
        | Step(h)::tail ->
            let r = stephelper h env
            lazyOptionSome r (rootListHelper tail env)

    match root with
    | CRN(rlist) -> rootListHelper rlist ((Map.empty, false) : Env)


let extract p str =
    match run pCrn str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> CRN([])

let program1 = extract pCrn "crn = { conc[A,2],  conc[A,2], step[add[A,B,C]]} "
printfn "%A" (TypeChecker program1)
let program2 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,C]]}"
printfn "%A" (TypeChecker program2)
let program3 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,A]]}"
printfn "%A" (TypeChecker program3)
let program4 = extract pCrn "crn = {conc[A,2], conc[B,2], step[add[A,B,C]]}"
printfn "%A" (TypeChecker program4)
let program5 = extract pCrn "crn = {conc[A,2], conc[B,2],  conc[C,0], step[add[A,B,C]]}"
printfn "%A" (TypeChecker program5)



