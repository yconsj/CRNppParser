#r "nuget: FParsec, 1.1.1"
open FParsec
type SPECIES = S of string


type MODULE = ADD of SPECIES*SPECIES*SPECIES | SUB of SPECIES* SPECIES * SPECIES | MUL of SPECIES* SPECIES* SPECIES | DIV of SPECIES*SPECIES*SPECIES
type COMMAND = MODULE
type COMMANDLIST = COMMAND | CMDLIST of COMMAND * COMMANDLIST

let (pCrn, pCrnRef) = createParserForwardedToRef<MODULE,unit>() 


 
let token p = p.>> spaces
let symbol s = token (pstring s)

let ident = 
        let charOrDigit c = isLetter c || isDigit c 
        spaces >>. token(many1Satisfy2L isLetter charOrDigit "identifier")   


let values1 = parse{
                let! a = ident
                let! _ = symbol ","
                let! b = ident
                return (a,b)
                }
let bValues1 = pstring "[" >>. values1 .>> pstring "]"

let values2 = parse{
                let! a = ident
                let! _ = symbol ","
                let! b = ident
                return (a,b)
                }
let bValues2 = pstring "[" >>. values2 .>> pstring "]"

let values3 = parse{
                let! a = ident
                let! _ = symbol ","
                let! b = ident
                let! _ = symbol ","
                let! c = ident
                return (a,b,c)
                }
let bValues3 = pstring "[" >>. values3 .>> pstring "]"
let pAdd = parse{
                let! _ = symbol "Add"
                let! (a,b,c) = bValues3
                if (a=c) || (c=b) then fail "error add" 
                else 

                    return ADD(S a,S b,S c)
            }
let pSub = parse{
                let! _ = symbol "Sub"
                let! (a,b,c) = bValues3
                if (a=c) || (c=b) then fail "error sub" 
                else 

                    return SUB(S a,S b,S c)
            }

let pMul = parse{
                let! _ = symbol "Mul"
                let! (a,b,c) = bValues3
                if (a=c) || (c=b) then fail "error mul" 
                else 

                    return MUL(S a,S b,S c)
            }
let pDiv = parse{
                let! _ = symbol "Div"
                let! (a,b,c) = bValues3
                if (a=c) || (c=b) then fail "error div" 
                else 

                    return DIV(S a,S b,S c)
            }
let pModule = spaces >>. (pAdd <|> pSub)

let pCommand = pModule

let pSpecies = parse{
                let! x = ident
                return (x)
                }
pCrnRef.Value <- parse {
            let! x  = pModule
            return x
        }
"A+B+C"
"ADD(A,B,C)"
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
test pCrn " Sub [A,B,C]"