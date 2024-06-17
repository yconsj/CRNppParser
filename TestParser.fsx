#r "nuget: FParsec, 1.1.1"
open FParsec
type Species = string
type Number = float

type ModuleS =
    | ADD of Species * Species * Species
    | SUB of Species * Species * Species
    | CMP of Species * Species

and CommandS =
    | CommandNested of CommandS * CommandS
    | ModuleS of ModuleS

type StepS = CommandS

type ConcS = Species * Number

type RootS =
    | RootNested of RootS * RootS
    | StepS of StepS
    | ConcS of ConcS

let (pCrn, pCrnRef) = createParserForwardedToRef<RootS, unit> ()


let token p = p .>> spaces
let symbol s = token (pstring s)

let ident =
    let charOrDigit c = isLetter c || isDigit c
    spaces >>. token (many1Satisfy2L isLetter charOrDigit "identifier")


let betweenBrackets p = between (pstring "[") (pstring "]") p
let betweenCurlyBrackets p = between (pstring "{") (pstring "}") p


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

let rec pCommandSopt (e : CommandS) =
    parse { let! _ = pstring ","
            let! e' = pCommandS
            return! pCommandSopt(CommandNested(e, e'))} 
    <|> preturn e
 
let pCommandSList =
    parse {let! e = pCommandS
        return! pCommandSopt e}


let pStepS =
    (pstring "step")
    >>. betweenBrackets (
        choice
            [ (attempt (pCommandS .>> (skipChar ',') .>>. pCommandSList)
               |>> fun (x, y) -> StepS(CommandNested(x, y)))
              attempt (pCommandS) |>> fun x -> StepS(x) ]
    )

let pConcS =
    (pstring "conc")
    >>. betweenBrackets ((spaces >>. ident .>> spaces .>> pchar ',') .>>. (spaces >>. pfloat .>> spaces))
    |>> fun (species, number) -> ConcS(species, number)

let pRootS = choice [ attempt pConcS; attempt pStepS ] |>> fun x -> x

let rec pRootsOpt (e : RootS) =
    parse { let! _ = pstring ","
            let! e' = pRootS
            return! pRootsOpt(RootNested(e, e'))} 
    <|> preturn e
 
let pRootSList =
    parse {let! e = pRootS
        return! pRootsOpt e}


pCrnRef.Value <-
    parse {
        // let! x = pRootS
        let! x =
            (pstring "crn= ") 
            >>. (betweenCurlyBrackets pRootSList)
        return x
    }

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pCrn "crn= {step[  add [A,B,C]]}"
test pCrn "crn= {step[  add [A,B,C], cmp[ A, C], sub [B,C,A] ]}"
