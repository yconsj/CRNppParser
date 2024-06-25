// @author Simon Janum
// @author August Valentin
// @date 26/6/2024
namespace CRNpp

module TypeChecker =
    open Parser
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
        if Option.isSome opt1 then opt1 else opt2

    type Env = Map<Species, Number> * CMPflag * STEPflag * CMPinStepflag // initialized variables, has executed a CMP, has executed a Step

    let typeChecker root =
        let isValidName (sp: Species) =
            if (Seq.head sp) = '_' then false else true

        let conchelper (conc: ConcS) (env: Env) =
            match conc, env with
            | _, (_, _, STEPflag(sflag), _) when sflag -> Some(sprintf "Cannot define concentrations after StepS"), env
            | (sp, _), (map, _, _, _) when Map.containsKey sp map -> // verify species hasn't been initialized yet.
                Some(sprintf "Preexisting species \'%s\' in env" sp), env
            | (sp, _), _ when not (isValidName sp) ->
                Some(sprintf "Name is not valid for \'%s\'. Name must not begin with '_'" sp), env
            | (sp, nu), (map, cflag, sflag, cthisflag) -> None, ((Map.add sp nu map), cflag, sflag, cthisflag)

        let IsSameSpecies sA sB =
            if (sA = sB) then
                Some(sprintf "Conflict in Module with Species: \'%s\'" sA)
            else
                None

        let triModHelper (a: Species) (b: Species) (c: Species) (env: Env) =
            (lazyOptionSome (IsSameSpecies a c) (IsSameSpecies b c)), env

        let binModHelper (a: Species) (b: Species) (env: Env) = IsSameSpecies a b, env

        let moduleHelper md (env: Env) =
            match md, env with
            | ADD(a, b, c), _
            | SUB(a, b, c), _
            | MUL(a, b, c), _
            | DIV(a, b, c), _ -> triModHelper a b c env //...
            | LD(a, b), _
            | SQRT(a, b), _ -> binModHelper a b env
            | CMP(a, b), (map, _, sflag, cthisflag) -> binModHelper a b (map, CMPflag(true), sflag, CMPinStepflag(true))

        let rxnhelper (rxn: RxnS) =
            match rxn with
            | (lhs, rhs, rate) when List.isEmpty lhs ->
                Some(sprintf "Rxn has empty left-hand side: \'%A -> %A : %f\'" lhs rhs rate)
            | _ -> None

        let rec stephelper (step: CommandS list) (env: Env) =
            match (step) with
            | [] -> None, env
            | Rxn(h) :: tail ->
                let rxnOpt = (rxnhelper h)
                let stepOpt, newEnv = (stephelper tail env)
                (lazyOptionSome (rxnOpt) (stepOpt)), newEnv
            | Module(h) :: tail ->
                let modOpt, newEnv = (moduleHelper h env)
                let stepOpt, newEnv = (stephelper tail newEnv)
                lazyOptionSome (modOpt) (stepOpt), newEnv
            | Conditional(h) :: tail ->
                let condOpt, newEnv = conditionalhelper h env
                let stepOpt, newEnv = (stephelper tail newEnv)
                lazyOptionSome (condOpt) (stepOpt), newEnv

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

        let rec rootListHelper rootList (env: Env) =
            match rootList, env with
            | [], (_, _, _, CMPinStepflag(cmpstepflag)) when cmpstepflag -> Some("cannot run CMP in last step.")
            | [], _ -> None
            | Conc(h) :: tail, _ ->
                let (r, newEnv) = conchelper h env
                lazyOptionSome r (rootListHelper tail newEnv)
            | Step(h) :: tail, ((map, cflag, sflag, cmpstepflag)) ->
                let stepOpt, newEnv = stephelper h (((map, cflag, sflag, CMPinStepflag(false))))
                lazyOptionSome stepOpt (rootListHelper tail newEnv)

        match root with
        | CRN(rlist) -> rootListHelper rlist ((Map.empty, CMPflag(false), STEPflag(false), CMPinStepflag(false)): Env)
