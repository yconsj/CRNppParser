#r "nuget: FParsec, 1.1.1"
open FParsec
type Species = string
type Number = float

type ModuleS =
    | ADD of Species * Species * Species
    | SUB of Species * Species * Species
    | CMP of Species * Species


type CommandS =
    | Module of ModuleS
    | Conditional of ConditionalS

and ConditionalS = IfGT of CommandSList

and CommandSList = CommandList of CommandS list

type StepS = Step of CommandSList

type ConcS = Conc of Species * Number

type RootS =
    | RootStep of StepS
    | RootConc of ConcS


type RootSList = RootList of RootS list


type Crn = Crn of RootSList


let (pCrn, pCrnRef) = createParserForwardedToRef<Crn, unit> ()



let token p = p .>> spaces
let symbol s = token (pstring s)

let ident =
    let charOrDigit c = isLetter c || isDigit c
    spaces >>. token (many1Satisfy2L isLetter charOrDigit "identifier")


let betweenBrackets p = between (pstring "[") (pstring "]") p

let betweenCurlyBrackets p =
    between (pstring "{" >>. spaces) (spaces >>. pstring "}") p

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
    choice [ spaces >>. pModuleS .>> spaces ] |>> fun (mod) -> Module (mod)


let pCommandSList =
      sepBy1 pCommandS (spaces >>. skipString "," >>. spaces) |>> fun l -> CommandList(l)

let pStepS =
    (pstring "step") >>. betweenBrackets (pCommandSList) |>> fun x -> Step(x)

let pConcS =
    (pstring "conc")
    >>. betweenBrackets ((spaces >>. ident .>> spaces .>> pchar ',') .>>. (spaces >>. pfloat .>> spaces))
    |>> fun (species, number) -> Conc(species, number)

let pRootS =
    choice
        [ attempt pConcS |>> fun x -> RootConc(x)
          attempt pStepS |>> fun x -> RootStep(x) ]

let pRootSList =

    sepBy1 pRootS (spaces >>. skipString "," >>. spaces) |>> fun l -> RootList(l)


pCrnRef.Value <-
    parse {
        let! x =
            pstring "crn"
            >>. spaces
            >>. pstring "="
            >>. spaces
            >>. betweenCurlyBrackets (pRootSList)

        return Crn(x)
    }

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pCrn "crn = { conc[A,2],  conc[A,2], step[add[A,B,C]]} "
