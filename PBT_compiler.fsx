#r "nuget: FsCheck, 3.0.0-rc3"
#r "nuget: FsUnit, 6.0.0"
#r "CRNpp/Library/net7.0/CRNpp.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open CRNpp.Compiler
open CRNpp.Parser
open CRNpp.Interpreter
open CRNpp.Simulator
open FsCheck
open FsCheck.FSharp
open FsUnit



let testProgram =
    "
    crn = {
    conc[c,3 ], conc[ cInitial ,3 ],
    conc[one ,1], conc[zero ,0],
    step[
    sub[c,one,cnext ]
                ]
     }  
    
    "

open System


let genVal =
    gen {
        let! r =
            Gen.where
                (fun (a, b, c) -> 60.0 > a && a >= 1.0 && 60.0 > b && b > 1.0 && 60.0 > c && c > 1.0)
                (Gen.three (ArbMap.defaults |> ArbMap.generate<float>))

        return r

    }

let genSub =
    gen {
        let! r =
            Gen.where
                (fun (a, b) -> 60.0 > a && a >= 1.0 && 60.0 > b && b > 1.0 && b - a >= -10 && abs (a - b) > 5)
                (Gen.two (ArbMap.defaults |> ArbMap.generate<float>))

        return r

    }

let geCondVal =
    gen {
        let! r =
            Gen.where
                (fun (a, b, c) ->
                    5.0 > a
                    && a >= 1.0
                    && 5.0 > b
                    && b > 1.0
                    && 5.0 > c
                    && c > 1.0
                    && abs (a - b) > 1)
                (Gen.three (ArbMap.defaults |> ArbMap.generate<float>))

        return r

    }

let testEuler (x: Number, y: Number, z: Number) =

    let program =
        "
        crn={
        conc[ f ,1], conc[one ,1], conc[ i , "
        + x.ToString()
        + " ],
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

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compiled = reactionSimulator i r 0.5


    let cSteady = Seq.item 1000 compiled
    let iSteady = Seq.item 200 interpreted

    let errorRate = 0.1

    Map.forall
        (fun k v ->
            if k = "_flag" then
                true
            else
                let x = Map.find k cSteady
                printfn "%A %A %A \n" k v x
                (abs (v - x) <= errorRate))
        iSteady

let testEQ (x: Number, y: Number, z: Number) =
    let concA = "conc[A," + x.ToString() + "]"
    let concB = "conc[B," + y.ToString() + "]"
    let concC = "conc[C," + "0" + "]"
    let step = "step[cmp[A,B]], step[ ifEQ [ add[A,B,C] ] ]"

    let program = "crn = { " + concA + "," + concB + "," + concC + "," + step + "}"

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compiled = reactionSimulator i r 0.1


    let cSteady = Seq.item 500 compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.1

    Map.forall
        (fun k v ->
            let x = Map.find k cSteady
            printfn "%A %A %A \n" k v x
            (abs (v - x) <= errorRate))
        iSteady

let testAdd (x: Number, y: Number, z: Number) =
    let concA = "conc[A," + x.ToString() + "]"
    let concB = "conc[B," + y.ToString() + "]"
    let concC = "conc[C," + z.ToString() + "]"
    let step = "step[ add[A,B,C] ]"

    let program = "crn = { " + concA + "," + concB + "," + concC + "," + step + "}"

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compiled = reactionSimulator i r 0.1


    let cSteady = Seq.item 500 compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.1

    Map.forall
        (fun k v ->
            let x = Map.find k cSteady

            (abs (v - x) <= errorRate))
        iSteady

let testMul (x: Number, y: Number, z: Number) =
    let concA = "conc[A," + x.ToString() + "]"
    let concB = "conc[B," + y.ToString() + "]"
    let concC = "conc[C," + z.ToString() + "]"
    let step = "step[ mul[A,B,C] ]"

    let program = "crn = { " + concA + "," + concB + "," + concC + "," + step + "}"

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compiled = reactionSimulator i r 0.1


    let cSteady = Seq.item 500 compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.5

    Map.forall
        (fun k v ->
            let x = Map.find k cSteady
            (abs (v - x) <= errorRate))
        iSteady

let testDiv (x: Number, y: Number, z: Number) =
    let concA = "conc[A," + x.ToString() + "]"
    let concB = "conc[B," + y.ToString() + "]"
    let concC = "conc[C," + z.ToString() + "]"
    let step = "step[ div[A,B,C] ]"

    let program = "crn = { " + concA + "," + concB + "," + concC + "," + step + "}"

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compiled = reactionSimulator i r 0.05


    let cSteady = Seq.item 500 compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.1

    Map.forall
        (fun k v ->
            let x = Map.find k cSteady
            (abs (v - x) <= errorRate))
        iSteady

let testSqrt (x: Number, y: Number) =
    let concA = "conc[A," + x.ToString() + "]"
    let concB = "conc[B," + y.ToString() + "]"
    let step = "step[ sqrt[A,B] ]"

    let program = "crn = { " + concA + "," + concB + "," + step + "}"

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compiled = reactionSimulator i r 0.1


    let cSteady = Seq.item 1000 compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.1

    Map.forall
        (fun k v ->
            let x = Map.find k cSteady
            (abs (v - x) <= errorRate))
        iSteady

let testSub (x: Number, y: Number) =
    let concA = "conc[A," + x.ToString() + "]"
    let concB = "conc[B," + y.ToString() + "]"
    let concC = "conc[C," + "0" + "]"
    let step = "step[ sub[A,B,C] ]"

    let program = "crn = { " + concA + "," + concB + "," + concC + "," + step + "}"

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compiled = reactionSimulator i r 0.01


    let cSteady = Seq.item 5000 compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 1.0

    Map.forall
        (fun k v ->
            let x = Map.find k cSteady
            (abs (v - x) <= errorRate))
        iSteady



type myGenerator =
    static member values() =
        { new Arbitrary<float * float * float>() with
            override x.Generator = genVal }

type subGenerator =
    static member values() =
        { new Arbitrary<float * float>() with
            override x.Generator = genSub }

type condGenerator =
    static member values() =
        { new Arbitrary<float * float * float>() with
            override x.Generator = geCondVal }

let config = Config.Quick.WithArbitrary([ typeof<myGenerator> ])
let configsub = Config.Quick.WithArbitrary([ typeof<subGenerator> ])
let configcond = Config.Quick.WithArbitrary([ typeof<condGenerator> ])
printfn "test EQ: \n"
Check.One(config, testEuler)
printfn "test add: \n"
Check.One(config, testAdd)
printfn "test sub: \n"
Check.One(configsub, testSub)
printfn "test mul: \n"
Check.One(config, testMul)
printfn "test div: \n"
Check.One(config, testDiv)
printfn "test sqrt: \n"
Check.One(configsub, testSqrt)
