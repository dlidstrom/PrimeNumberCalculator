namespace Prime.Core.Tests

module Tests =

    open Expecto

    [<Tests>]
    let tests =
        testList "samples" [
            testCase "universe exists" <| fun _ ->
              let subject = true
              Expect.isTrue subject "I compute, therefore I am."

            testProperty "Product is distributive over addition" <|
              fun a b c ->
                a * (b + c) = a * b + a * c
        ]
