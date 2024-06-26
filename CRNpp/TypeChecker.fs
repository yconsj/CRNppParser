// @author Simon Janum
// @author August Valentin
// @date 26/6/2024
namespace CRNpp

module TypeChecker =
    open Parser
    open FParsec
    open System

    /// TypeChecker:
    /// Check the explicit restrictions: ✔
    /// Check CMP is executed in a step prior to ConditionalS: ✔
    /// Dont allow CMP in last step: ✔
    /// Don't allow conditional in the same step as CMP: ✔
    /// dont allow names starting with underscore (protected names): ✔
    /// dont allow empty LHS on Rxn: ✔
    /// dont allow Conc on the same species multiple times: ✔
    /// a ConcS cannot follow after StepS: ✔

    type Flags(cmpFlag: bool, stepFlag: bool, cmpInStepFlag: bool, cndInStepFlag: bool) =

        member this.Cmp = cmpFlag // has executed a CMP
        member this.Step = stepFlag // has executed a Step
        member this.CmpInStep = cmpInStepFlag // has executed a CMP in this step
        member this.CndInStep = cndInStepFlag // has executed a conditional in this step

    let lazyOptionSome opt1 opt2 =
        if Option.isSome opt1 then opt1 else opt2


    type Env = Set<Species> * Flags

    let TypeChecker root =
        let isValidName (sp: Species) = not ((Seq.head sp) = '_')

        let conchelper (conc: ConcS) (env: Env) =
            match conc, env with
            | _, (_, flags) when flags.Step -> Some(sprintf "Cannot define concentrations after StepS"), env
            | (sp, _), (set, _) when Set.contains sp set -> // verify species hasn't been initialized yet.
                Some(sprintf "Preexisting species \'%s\' in env" sp), env
            | (sp, _), _ when not (isValidName sp) ->
                Some(sprintf "Name is not valid for \'%s\'. Name must not begin with '_'" sp), env
            | (sp, nu), (set, flags) -> None, ((Set.add sp set), flags)

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
            | CMP(a, b), (set, flags) ->
                if flags.CndInStep then
                    Some(sprintf "CMP may not be executed in the same step as conditional"), env
                else
                    let newFlags = Flags(true, flags.Step, true, flags.CndInStep)
                    binModHelper a b (set, newFlags)

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
            | IfGT(cmds), (_, flags)
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

        let rec rootListHelper rootList (env: Env) =
            match rootList, env with
            | [], (_, flags) when flags.CmpInStep -> Some("cannot run CMP in last step.")
            | [], _ -> None
            | Conc(h) :: tail, _ ->
                let (r, newEnv) = conchelper h env
                lazyOptionSome r (rootListHelper tail newEnv)
            | Step(h) :: tail, ((set, flags)) ->
                let newFlags = Flags(flags.Cmp, true, false, false)
                let stepOpt, newEnv = stephelper h (set, newFlags)
                lazyOptionSome stepOpt (rootListHelper tail newEnv)

        match root with
        | CRN(rlist) -> rootListHelper rlist (Set.empty, Flags(false, false, false, false): Env)
