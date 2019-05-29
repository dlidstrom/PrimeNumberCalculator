open System
open BigIntegerExtensions
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

[<SimpleJob(
    launchCount = 1,
    warmupCount = 3,
    targetCount = 5,
    invocationCount = 1000,
    id = "QuickJob")>]
type Comparison() =
    member this.Args() =
        seq {
            yield 100I
            yield 1000I
            yield 10000I
            yield 100000I
            yield 1000000I
            yield 10000000I
            yield 100000000I
            yield 1000000000I
            yield 10000000000I
            yield 100000000000I
            yield 1000000000000I
            yield 10000000000000I
            yield 100000000000000I
            yield 1000000000000000I
            yield 10000000000000000I
            yield 100000000000000000I
            yield 1000000000000000000I
            yield 10000000000000000000I
            yield 100000000000000000000I
            yield 1000000000000000000000I
            yield 10000000000000000000000I
            yield 100000000000000000000000I
            yield 1000000000000000000000000I
            yield 10000000000000000000000000I
            yield 100000000000000000000000000I
            yield 1000000000000000000000000000I
            yield 10000000000000000000000000000I
            yield 100000000000000000000000000000I
            yield 1000000000000000000000000000000I
        }

    [<Benchmark>]
    [<ArgumentsSource("Args")>]
    member this.Sqrt1(n) = bigintSqrt n

    [<Benchmark(Baseline = true)>]
    [<ArgumentsSource("Args")>]
    member this.Sqrt2(n) = sqrt n

[<EntryPoint>]
let main _ =
    let summary = BenchmarkRunner.Run<Comparison>()
    0
