//#r "CRNppInterpreter\\Parser.fs"
#r "CRNppInterpreter\\Library\\net7.0\\CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
open Interperter.Parser
open FParsec

/// TypeChecker:
/// Check the explicit restrictions
/// Check CMP is executed in a step prior to ConditionalS
/// (Not true:) only non-conflicting commands in Steps
/// only initialized (conc) species allowed in (source-variable of?) modules
/// a conc cannot follow after StepS(?)

let lazyOptionSome opt1 opt2 =
    if Option.isSome opt1 then 
        opt1 
    else
        opt2

type Env = Set<Species> * bool // initialized variables, has run a CMP

let TypeChecker root = 
    let conchelper conc m =
        match conc with
        | Conc(sp, nu) -> 
            if (Map.containsKey sp m) then
                Some(sprintf "Preexisting species \'%s\' in env" sp), m
            else
                None, (Map.add sp nu m)
    let IsNotInitializedVariable vars (m : Map<Species,Number>) =
        let uninitialized = Seq.tryFind (fun x -> not (Map.containsKey x m)) vars
        if (Option.isSome uninitialized) then
            Some(sprintf "Reference to uninitialized Species: \'%s\'" (Option.get uninitialized))
        else
            None

    let IsSameSpecies sA sB =
        if (sA = sB) then
            Some(sprintf "Conflict in Module with Species: \'%s\'" sA)
        else
            None
            
    let triModHelper (a : Species) (b : Species) (c : Species) m =
        let vars = seq {a;b;c}
        let uninitialized = (IsNotInitializedVariable vars m)
        let AeqC = IsSameSpecies a c
        lazyOptionSome (lazyOptionSome uninitialized AeqC) (IsSameSpecies b c)
    
    let binModHelper (a : Species) (b : Species) m =
        let vars = seq {a;b}
        let uninitialized = (IsNotInitializedVariable vars m)
        let Aeqb = IsSameSpecies a b
        lazyOptionSome uninitialized Aeqb

    let moduleHelper md m =
        match md with
        | ADD(a,b,c)
        | SUB(a,b,c)
        | MUL(a,b,c)
        | DIV(a,b,c) -> triModHelper a b c m //...
        | LD(a,b)
        | SQRT(a,b)
        | CMP(a,b) -> binModHelper a b m 



    let rec stephelper (step : CommandS list ) m =
        match (step) with
        | [] -> None
        | Module(h)::tail -> lazyOptionSome (moduleHelper h m) (stephelper tail m)
        | Conditional(h)::tail ->
            let r = conditionalhelper h m
            lazyOptionSome r (stephelper tail m)
    and conditionalhelper cond m =
        match cond with
        | IfGT(CommandList(cmds)) 
        | IfGE(CommandList(cmds))
        | IfEQ(CommandList(cmds))
        | IfLT(CommandList(cmds))
        | IfLE(CommandList(cmds)) -> stephelper cmds m

    let rec rootListHelper rootList m =
        match rootList with
        | []-> None
        | RootConc(h)::tail -> 
            let (r,m) = conchelper h m
            lazyOptionSome r (rootListHelper tail m)
        | RootStep(Step(CommandList(h)))::tail ->
            let r = stephelper h m
            lazyOptionSome r (rootListHelper tail m)

    match root with
    | Crn(RootList(rlist)) -> rootListHelper rlist Map.empty


let extract p str =
    match run pCrn str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> Crn(RootList([]))

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



