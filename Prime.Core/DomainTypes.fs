module DomainTypes

type Classification =
    | Composite of bigint
    | Prime of bigint
    | ProbablePrime of bigint
    | Invalid of bigint

type PrimalityResult =
    | Known of Classification
    | Unknown of bigint
module PrimalityResult =
    let bind f e =
        match e with
        | Known e -> Known e
        | Unknown e -> f(e)
