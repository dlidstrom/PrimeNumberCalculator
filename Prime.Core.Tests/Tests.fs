module Tests

open DomainTypes
open MillerRabin
open Expecto

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

module Helpers =
    let createTestList name list (expected) =
        testList
            name
            [
                for i in list do
                    yield testCase
                        (sprintf "%A" i)
                        <| fun () ->
                            let classification = expected i
                            match classification with
                            | Known k ->
                                match k with
                                | Composite _ ->
                                    Expect.equal
                                        (isPrime i)
                                        classification
                                        "Expected Composite"
                                | Prime _ ->
                                    Expect.equal
                                        (isPrime i)
                                        classification
                                        "Expected Prime"
                                | ProbablePrime _ ->
                                    Expect.equal
                                        (isPrime i)
                                        classification
                                        "Expected ProbablePrime"
                                | Invalid _ -> failwith "Invalid"
                            | _ -> failwith "Unknown"
            ]

[<Tests>]
let smallPrimes =
    let expected x = Prime x |> Known
    Helpers.createTestList
        "Small Primes"
        Constants.SmallPrimes
        expected

[<Tests>]
let mediumPrimes =
    let expected x = Prime x |> Known
    Helpers.createTestList
        "Medium Primes"
        Constants.MediumPrimes
        expected

[<Tests>]
let largePrimes =
    let expected x = Prime x |> Known
    Helpers.createTestList
        "Large Primes"
        Constants.LargePrimes
        expected

[<Tests>]
let smallMediumComposites =
    let expected x = Composite x |> Known
    Helpers.createTestList
        "Small Medium Composites"
        [for x in Constants.SmallPrimes do
            for y in Constants.MediumPrimes -> x * y]
        expected

[<Tests>]
let smallLargeComposites =
    let expected x = Composite x |> Known
    Helpers.createTestList
        "Small Large Composites"
        [for x in Constants.SmallPrimes do
            for y in Constants.LargePrimes -> x * y]
        expected

[<Tests>]
let mediumLargeComposites =
    let expected x = Composite x |> Known
    Helpers.createTestList
        "Medium Large Composites"
        [for x in Constants.MediumPrimes do
            for y in Constants.LargePrimes -> x * y]
        expected
