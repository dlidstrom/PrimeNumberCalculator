namespace Prime.Core.Tests

module Tests =

    open Expecto
    open Prime.Core

    let isPrime x = x > 3

    [<Tests>]
    let tests =
        testList "samples"
            [
                for i in [2; 3; 5; 7] do
                    yield testCase (sprintf "isPrime %d" i) <| fun () ->
                        Expect.isTrue (isPrime i) "str"
            ]
