module Parser
#r "nuget: FParsec, 1.1.1"
open FParsec
type Species = string
type Number = float

type ModuleS =
    | LD of Species * Species
    | ADD of Species * Species * Species
    | SUB of Species * Species * Species
    | MUL of Species * Species * Species
    | DIV of Species * Species * Species
    | SQRT of Species * Species
    | CMP of Species * Species


type CommandS =
    | Module of ModuleS 
    | Conditional of ConditionalS

and ConditionalS =
    | IfGT of CommandSList
    | IfGE of CommandSList
    | IfEQ of CommandSList
    | IfLT of CommandSList
    | IfLE of CommandSList

and CommandSList = CommandS list

type StepS = CommandSList

type ConcS = Species * Number

type RootS =
    | Step of StepS
    | Conc of ConcS


type RootSList = RootS list


type Crn = CRN of RootSList


let (pCrn, pCrnRef) = createParserForwardedToRef<Crn, unit> ()

// Forward declaration of pCommandS and pCommandSList

let pCommandSList, pCommandSListRef = createParserForwardedToRef ()

let token p = p .>> spaces
let symbol s = token (pstring s)

let ident =
    let charOrDigit c = isLetter c || isDigit c
    spaces >>. token (many1Satisfy2L isLetter charOrDigit "identifier")


let betweenBrackets p = between (pstring "[") (pstring "]") p

let betweenCurlyBrackets p =
    between (pstring "{") (spaces >>. pstring "}") p

let commaSeparated2 p1 p2 = tuple2 p1 (pchar ',' >>. spaces >>. p2)

let commaSeparated3 p1 p2 p3 =
    tuple3 p1 (pchar ',' >>. spaces >>. p2) (pchar ',' >>. spaces >>. p3)

let pLd =
    pstring "ld" >>. spaces >>. betweenBrackets (commaSeparated2 ident ident)
    |>> fun (s1, s2) -> LD(s1, s2)

let pAdd =
    pstring "add" >>. spaces >>. betweenBrackets (commaSeparated3 ident ident ident)
    |>> fun (s1, s2, s3) -> ADD(s1, s2, s3)

let pSub =
    pstring "sub" >>. spaces >>. betweenBrackets (commaSeparated3 ident ident ident)
    |>> fun (s1, s2, s3) -> SUB(s1, s2, s3)

let pMul =
    pstring "mul" >>. spaces >>. betweenBrackets (commaSeparated3 ident ident ident)
    |>> fun (s1, s2, s3) -> MUL(s1, s2, s3)

let pDiv =
    pstring "div" >>. spaces >>. betweenBrackets (commaSeparated3 ident ident ident)
    |>> fun (s1, s2, s3) -> DIV(s1, s2, s3)

let pSqrt =
    pstring "sqrt" >>. spaces >>. betweenBrackets (commaSeparated2 ident ident)
    |>> fun (s1, s2) -> SQRT(s1, s2)

let pCmp =
    pstring "cmp" >>. spaces >>. betweenBrackets (commaSeparated2 ident ident)
    |>> fun (s1, s2) -> CMP(s1, s2)


let pModuleS =
    spaces >>. choice [ pCmp; pSub; pAdd; pLd; pSqrt; pDiv; pMul ]
    |>> fun (arith) -> arith




let pIfGT =
    spaces >>. pstring "ifGT" >>. spaces >>. betweenBrackets (pCommandSList)
    |>> fun x -> IfGT(x)

let pIfGE =
    spaces >>. pstring "ifGE" >>. spaces >>. betweenBrackets (pCommandSList)
    |>> fun x -> IfGE(x)

let pIfEQ =
    spaces >>. pstring "ifEQ" >>. spaces >>. betweenBrackets (pCommandSList)
    |>> fun x -> IfEQ(x)

let pIfLT =
    spaces >>. pstring "ifLT" >>. spaces >>. betweenBrackets (pCommandSList)
    |>> fun x -> IfLT(x)

let pIfLE =
    spaces >>. pstring "ifLE" >>. spaces >>. betweenBrackets (pCommandSList)
    |>> fun x -> IfLE(x)

let pConditionalS =
    choice
        [ pIfGT |>> fun x -> x
          pIfEQ |>> fun x -> x
          pIfLT |>> fun x -> x
          pIfLE |>> fun x -> x ]

let pCommandS =
    spaces
    >>. choice
        [ pConditionalS .>> spaces |>> fun cond -> Conditional(cond)
          pModuleS .>> spaces |>> fun (mod) -> Module (mod) ]

let pCommandSListImpl =
    sepBy1 pCommandS (spaces >>. skipString "," >>. spaces) |>> fun l -> l

pCommandSListRef.Value <- pCommandSListImpl



let pStepS =
    (pstring "step") >>. spaces >>. betweenBrackets (pCommandSList) |>> fun x -> x

let pConcS =
    (pstring "conc")
    >>. spaces
    >>. betweenBrackets ((spaces >>. ident .>> spaces .>> pchar ',') .>>. (spaces >>. pfloat .>> spaces))
    |>> fun (species, number) -> species, number

let pRootS =
    spaces
    >>. choice [ (pConcS |>> fun x -> Conc(x)); (pStepS |>> fun x -> Step(x)) ]
    .>> spaces

let pRootSList =

    sepBy1 pRootS (skipString ",") |>> fun l -> l


pCrnRef.Value <-
    parse {
        let! x =
            spaces
            >>. pstring "crn"
            >>. spaces
            >>. pstring "="
            >>. spaces
            >>. betweenCurlyBrackets (pRootSList)

        return CRN(x)
    }

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pCrn " crn = { conc[A,2] , conc [ A, 2 ],step [ add [ A , B , C ] ] } "

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
    ""