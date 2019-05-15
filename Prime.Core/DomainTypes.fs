module DomainTypes

type Classification =
    | Composite of bigint
    | Prime of bigint
    | ProbablePrime of bigint
    | Invalid of bigint
module Classification =
    let value =
        function
        | Composite c -> c
        | Prime p -> p
        | ProbablePrime p -> p
        | Invalid p -> p

type PrimalityResult =
    | Known of Classification
    | Unknown of bigint
module PrimalityResult =
    let bind f e =
        match e with
        | Known e -> Known e
        | Unknown e -> f(e)
