﻿// Learn more about F# at http://fsharp.org

open System
open BigIntegerExtensions
open BenchmarkDotNet

type Comparison() =
    [<Benchmark>]
    member this.Sqrt1 = bigintSqrt 1000I

[<EntryPoint>]
let main _ =
    printfn "Hello World from F#!"
    0 // return an integer exit code
