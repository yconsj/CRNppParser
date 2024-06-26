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

open System.Globalization
// required to generate the expected float format:
CultureInfo.CurrentCulture <- CultureInfo.GetCultureInfo("en-US")



let genVal =
    gen {
        let! r =
            Gen.where
                (fun (a, b, c) -> 30.0 > a && a >= 1.0 && 30.0 > b && b > 1.0 && 30.0 > c && c > 1.0)
                (Gen.three (ArbMap.defaults |> ArbMap.generate<float>))

        return r

    }

let genSub =
    gen {
        let! r =
            Gen.where
                (fun (a, b) -> 30.0 > a && a >= 1.0 && 30.0 > b && b > 1.0 && b - a >= -5 && abs (a - b) > 5)
                (Gen.two (ArbMap.defaults |> ArbMap.generate<float>))

        return r

    }

let genCondVal =
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

let testAdd (x: Number, y: Number, z: Number) =
    let concA = "conc[A," + x.ToString() + "]"
    let concB = "conc[B," + y.ToString() + "]"
    let concC = "conc[C," + z.ToString() + "]"
    let step = "step[ add[A,B,C] ]"

    let program = "crn = { " + concA + "," + concB + "," + concC + "," + step + "}"

    let interpreted = parseAndInterpret program
    let (r, i) = (parseAndCompile program)
    let compStepSize = 0.1
    let compSteps = 250.0
    let compiled = reactionSimulator i r compStepSize

    let cSteady = Seq.item (int (compSteps / compStepSize)) compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.5

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
    let compStepSize = 0.1
    let compSteps = 250.0
    let compiled = reactionSimulator i r compStepSize

    let cSteady = Seq.item (int (compSteps / compStepSize)) compiled
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

    let compStepSize = 0.1
    let compSteps = 250.0
    let compiled = reactionSimulator i r compStepSize

    let cSteady = Seq.item (int (compSteps / compStepSize)) compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.5

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

    let compStepSize = 0.1
    let compSteps = 250.0
    let compiled = reactionSimulator i r compStepSize

    let cSteady = Seq.item (int (compSteps / compStepSize)) compiled
    let iSteady = Seq.item 1 interpreted

    let errorRate = 0.5

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

    let compStepSize = 0.1
    let compSteps = 250.0
    let compiled = reactionSimulator i r compStepSize


    let cSteady = Seq.item (int (compSteps / compStepSize)) compiled
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

let config = Config.Quick.WithArbitrary([ typeof<myGenerator> ])
let configsub = Config.Quick.WithArbitrary([ typeof<subGenerator> ])

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
