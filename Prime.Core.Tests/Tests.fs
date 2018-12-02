namespace Prime.Core.Tests

module Tests =

    open Expecto
    open Prime.Core.MillerRabin
    open Prime.Core.DomainTypes

    [<Tests>]
    let tests =
        testList "samples"
            [
                for i in [2; 3; 5; 7] do
                    yield testCase (sprintf "isPrime %d" i) <| fun () ->
                        let actual = isPrime i
                        Expect.equal actual Primality.Prime "Expected prime"
            ]
