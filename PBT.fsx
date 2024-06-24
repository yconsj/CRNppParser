#r "nuget: FsCheck, 3.0.0-rc3"
#r "nuget: FsUnit, 6.0.0"
#r "CRNppInterpreter/Library/net7.0/CRNppInterpreter.dll"
#r "nuget: FParsec, 1.1.1"
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: MathNet.Numerics, 5.0.0"

open Interpreter.Compiler
open Interpreter.Parser
open Interpreter.Interpreter
open Interpreter.Simulator
open FsCheck
open FsCheck.FSharp
open FsUnit



//reactionSimulatorPlot initial reactions 0.5 1200



//
open System


let genLD =
    gen {
        let! r =
            Gen.where
                (fun (a, b, c) ->
                    a <> b
                    && b <> c
                    && a <> c
                    && 100.0 > a
                    && a > 1.0
                    && 100.0 > b
                    && b > 1.0
                    && 100.0 > c
                    && c > 1.0)
                (Gen.three (ArbMap.defaults |> ArbMap.generate<float>))
        // avoid restrictions
        return r

    }

let testLD (x: Number, y: Number, z: Number) =
    let m = Map.ofList [ ("A", x); ("B", y) ]
    let l = [ RxnS([ "A" ], [ "A"; "B" ], 1) ] @ [ RxnS([ "B" ], [], 1) ]


    let s = reactionSimulator m l 1
    let lastMap = Seq.last (Seq.take 50 s)
    let a = Map.find "A" lastMap
    let b = Map.find "B" lastMap

    let errorRate = 0.000001
    (abs (b - x) <= errorRate)

let testAdd (x: Number, y: Number, z: Number) =
    let m = Map.ofList [ ("A", x); ("B", y); ("C", z) ]

    let l =
        [ RxnS([ "A" ], [ "A"; "C" ], 1) ]
        @ [ RxnS([ "B" ], [ "B"; "C" ], 1) ]
        @ [ RxnS([ "C" ], [], 1) ]

    let s = reactionSimulator m l 1
    let lastMap = Seq.last (Seq.take 50 s)
    let a = Map.find "A" lastMap
    let b = Map.find "B" lastMap
    let c = Map.find "C" lastMap

    let errorRate = 0.01
    (abs (c - (x + y)) <= errorRate)

let testSub (x: Number, y: Number, z: Number) =
    let m = Map.ofList [ ("A", x); ("B", y); ("C", z); ("H", 0) ]

    let l =
        [ RxnS([ "A" ], [ "A"; "C" ], 1) ]
        @ [ RxnS([ "B" ], [ "B"; "H" ], 1) ]
        @ [ RxnS([ "C" ], [], 1) ]
        @ [ RxnS([ "C"; "H" ], [], 1) ]

    let s = reactionSimulator m l 1
    let lastMap = Seq.last (Seq.take 50 s)
    let a = Map.find "A" lastMap
    let b = Map.find "B" lastMap
    let c = Map.find "C" lastMap

    let errorRate = 0.1

    printfn "%A\n" lastMap

    if x > y then
        (abs (c - (x - y)) <= errorRate)
    else
        abs (c) <= errorRate

let testMul (x: Number, y: Number, z: Number) =
    let m = Map.ofList [ ("A", x); ("B", y); ("C", z) ]
    let l = [ RxnS([ "A"; "B" ], [ "A"; "B"; "C" ], 1) ] @ [ RxnS([ "C" ], [], 1) ]

    let s = reactionSimulator m l 1
    let lastMap = Seq.last (Seq.take 50 s)
    let a = Map.find "A" lastMap
    let b = Map.find "B" lastMap
    let c = Map.find "C" lastMap

    let errorRate = 0.01
    (abs (c - (x * y)) <= errorRate)

let testDiv (x: Number, y: Number, z: Number) =
    let m = Map.ofList [ ("A", x); ("B", y); ("C", z) ]
    let l = [ RxnS([ "A" ], [ "A"; "C" ], 1) ] @ [ RxnS([ "B"; "C" ], [ "B" ], 1) ]

    let s = reactionSimulator m l 1
    let lastMap = Seq.last (Seq.take 50 s)
    let a = Map.find "A" lastMap
    let b = Map.find "B" lastMap
    let c = Map.find "C" lastMap

    let errorRate = 0.01
    printfn "%A\n" (abs (c - (x / y)))
    (abs (c - (x / y)) <= errorRate)

let testSqrt (x: Number, y: Number, z: Number) =
    let m = Map.ofList [ ("A", x); ("B", y) ]
    let l = [ RxnS([ "A" ], [ "A"; "B" ], 1) ] @ [ RxnS([ "B"; "B" ], [], 0.5) ]

    let s = reactionSimulator m l 1
    let lastMap = Seq.last (Seq.take 50 s)
    let a = Map.find "A" lastMap
    let b = Map.find "B" lastMap

    let errorRate = 0.1
    printfn "%A\n" s
    (abs (b - (sqrt (a))) <= errorRate)


type myGenerator =
    static member values() =
        { new Arbitrary<float * float * float>() with
            override x.Generator = genLD }



let config = Config.Quick.WithArbitrary([ typeof<myGenerator> ])

Check.One(config, testSqrt)
//printfn "%A" (Gen.sample 1 genLD)
printfn "test ld: \n"
Check.One(config, testLD)
printfn "test add: \n"
Check.One(config, testAdd)
printfn "test sub: \n"
Check.One(config, testSub)
printfn "test mul: \n"
Check.One(config, testMul)
printfn "test div: \n"
Check.One(config, testDiv)
printfn "test sqrt: \n"
