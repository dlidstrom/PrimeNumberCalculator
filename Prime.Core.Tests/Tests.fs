namespace Prime.Core.Tests

module Tests =

    open Expecto
    open Prime.Core.MillerRabin
    open Prime.Core.DomainTypes

    [<RequireQualifiedAccess>]
    module Constants =
        let SmallPrimes = [
             2I;  3I;  5I;  7I; 11I; 13I; 17I;
            19I; 23I; 29I; 31I; 37I; 41I;
            43I; 47I; 53I; 59I; 61I; 67I;
            71I; 73I; 79I; 83I; 89I; 97I
        ]

        let MediumPrimes = [
            7727I; 7919I; 125003I;
            999331I; 27644437I; 16769023I
        ]

        let LargePrimes = [
            87178291199I;
            10888869450418352160768000001I;
            265252859812191058636308479999999I;
            263130836933693530167218012159999999I
            8683317618811886495518194401279999999I
        ]

    [<RequireQualifiedAccess>]
    module Helpers =
        let createTestList name list expected =
            testList
                name
                [
                    for i in list do
                        yield testCase
                            (sprintf "%A" i)
                            <| fun () ->
                                match expected with
                                | Primality.Composite ->
                                    Expect.equal
                                        (isPrime i)
                                        expected
                                        "Expected Composite"
                                | Primality.Prime ->
                                    Expect.equal
                                        (isPrime i)
                                        expected
                                        "Expected Prime"
                ]

    [<Tests>]
    let smallPrimes =
        Helpers.createTestList
            "Small Primes"
            Constants.SmallPrimes
            Primality.Prime

    [<Tests>]
    let mediumPrimes =
        Helpers.createTestList
            "Medium Primes"
            Constants.MediumPrimes
            Primality.Prime

    [<Tests>]
    let largePrimes =
        Helpers.createTestList
            "Large Primes"
            Constants.LargePrimes
            Primality.Prime

    [<Tests>]
    let smallMediumComposites =
        Helpers.createTestList
            "Small Medium Composites"
            [for x in Constants.SmallPrimes do
                for y in Constants.MediumPrimes -> x * y]
            Primality.Composite

    [<Tests>]
    let smallLargeComposites =
        Helpers.createTestList
            "Small Large Composites"
            [for x in Constants.SmallPrimes do
                for y in Constants.LargePrimes -> x * y]
            Primality.Composite

    [<Tests>]
    let mediumLargeComposites =
        Helpers.createTestList
            "Medium Large Composites"
            [for x in Constants.MediumPrimes do
                for y in Constants.LargePrimes -> x * y]
            Primality.Composite
