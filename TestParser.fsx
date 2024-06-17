#r "nuget: FParsec, 1.1.1"
open FParsec
type Species = string
type Number = float

type ModuleS =
    | ADD of Species * Species * Species
    | SUB of Species * Species * Species
    | CMP of Species * Species


type CommandS = ModuleS of ModuleS

and CommandSopt =
    | Comma of CommandS * CommandSopt
    | Empty

type StepS = CommandS * CommandSopt

type ConcS = Species * Number

type RootS =
    | StepS of StepS
    | ConcS of ConcS



let (pCrn, pCrnRef) = createParserForwardedToRef<RootS, unit> ()



let token p = p .>> spaces
let symbol s = token (pstring s)

let ident =
    let charOrDigit c = isLetter c || isDigit c
    spaces >>. token (many1Satisfy2L isLetter charOrDigit "identifier")


let betweenBrackets p = between (pstring "[") (pstring "]") p

let commaSeparated2 p1 p2 = tuple2 p1 (pchar ',' >>. spaces >>. p2)

let commaSeparated3 p1 p2 p3 =
    tuple3 p1 (pchar ',' >>. spaces >>. p2) (pchar ',' >>. spaces >>. p3)

let pAdd =
    spaces
    >>. pstring "add"
    >>. spaces
    >>. betweenBrackets (commaSeparated3 ident ident ident)
    |>> fun (s1, s2, s3) -> ADD(s1, s2, s3)

let pSub =
    spaces
    >>. pstring "sub"
    >>. spaces
    >>. betweenBrackets (commaSeparated3 ident ident ident)
    |>> fun (s1, s2, s3) -> SUB(s1, s2, s3)

let pCmp =
    spaces
    >>. pstring "cmp"
    >>. spaces
    >>. betweenBrackets (commaSeparated2 ident ident)
    |>> fun (s1, s2) -> CMP(s1, s2)

let pModuleS =
    choice [ attempt pCmp; attempt pSub; attempt pAdd ] |>> fun (arith) -> arith

let pCommandS =
    choice [ spaces >>. pModuleS .>> spaces ] |>> fun (mod) -> ModuleS (mod)

let pCommandSopt =

    let rec helper l =
        match l with
        | x :: [] -> Comma(x, Empty)
        | x :: y -> Comma(x, helper y)
        | _ -> Empty

    sepBy pCommandS (skipString ",") |>> fun l -> helper l

let pStepS =
    (pstring "step")
    >>. betweenBrackets (
        choice
            [ (attempt (pCommandS .>> (skipChar ',') .>>. pCommandSopt)
               |>> fun (x, y) -> StepS(x, y))
              attempt (pCommandS) |>> fun x -> StepS(x, Empty) ]
    )

let pConcS =
    (pstring "conc")
    >>. betweenBrackets ((spaces >>. ident .>> spaces .>> pchar ',') .>>. (spaces >>. pfloat .>> spaces))
    |>> fun (species, number) -> ConcS(species, number)

let pRootS = choice [ attempt pConcS; attempt pStepS ] |>> fun x -> x

pCrnRef.Value <-
    parse {
        let! x = pRootS
        return x
    }

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pCrn "step[  add [A,B,C]]"
