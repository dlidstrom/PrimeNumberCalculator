module MillerRabin

open DomainTypes

let private smallPrimes =
    [  2I;  3I;  5I;  7I; 11I; 13I; 17I; 19I; 23I; 29I;
      31I; 37I; 41I; 43I; 47I; 53I; 59I; 61I; 67I; 71I;
      73I; 79I; 83I; 89I; 97I; 101I; 103I; 107I; 109I; 113I;
      127I; 131I; 137I; 139I; 149I; 151I; 157I; 163I; 167I; 173I;
      179I; 181I; 191I; 193I; 197I; 199I; 211I; 223I; 227I; 229I;
      233I; 239I; 241I; 251I; 257I ]
    |> Set.ofList

/// Returns true if probable prime; false if composite.
let private innerMillerRabin num a s m d =
    let firstModPow = bigint.ModPow(a, d, num)
    if firstModPow = 1I || firstModPow = m
    then
        true
    else
        let rec tryPow j =
            if j < s
            then
                let exp = bigint.Pow(2I, j) * d
                let modPow = bigint.ModPow(a, exp, num)
                if modPow = m
                then
                    true
                else
                    tryPow (j + 1)
            else
                false

        tryPow 1

let checkBases num =
    let proofBases =
        match num with
        | prim when prim < 1373653I ->
            Prime prim, [ 2I; 3I ] |> Seq.ofList
        | prim when prim < 9080191I ->
            Prime prim, [ 31I; 73I ] |> Seq.ofList
        | prim when prim < 25326001I ->
            Prime prim, [ 2I; 3I; 5I ] |> Seq.ofList
        | prim when prim < 4759123141I ->
            Prime prim, [ 2I; 7I; 61I ] |> Seq.ofList
        | prim when prim < 1122004669633I ->
            Prime prim, [ 2I; 13I; 23I; 1662803I ] |> Seq.ofList
        | prim when prim < 2152302898747I ->
            Prime prim, [ 2I; 3I; 5I; 7I; 11I ] |> Seq.ofList
        | prim when prim < 3474749660383I ->
            Prime prim, [ 2I; 3I; 5I; 7I; 11I; 13I ] |> Seq.ofList
        | prim when prim < 341550071728321I ->
            Prime prim, [ 2I; 3I; 5I; 7I; 11I; 13I; 17I ] |> Seq.ofList
        | prim when prim < 3825123056546413051I ->
            Prime prim, [ 2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I ] |> Seq.ofList
        // | prim when prim < 18446744073709551616I ->
        //     Prime prim, [ 2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I; 29I; 31I; 37I ] |> Seq.ofList
        | prim when prim < 318665857834031151167461I ->
            Prime prim, [ 2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I; 29I; 31I; 37I ] |> Seq.ofList
        | prim when prim < 3317044064679887385961981I ->
            Prime prim, [ 2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I; 29I; 31I; 37I; 41I ] |> Seq.ofList
        | prim ->
            ProbablePrime prim, seq {
                for i in [ 1 .. 20 ] ->
                    BigIntegerExtensions.randomIntegerBelow (num - 2I) }

    // todo: document what is s
    let m = num - 1I
    let calcS m =
        let rec inner s (n : bigint) =
            if n.IsEven
            then inner (s + 1) (bigint.Divide(n, 2I))
            else s
        inner 0 m

    let s = calcS m
    let d = m / bigint.Pow(2I, s)

    // both cases are really the same
    // if array is empty, fill with some random bigint's (lazily?)
    let proof, bases = proofBases
    let passesAllBases =
        bases
        |> Seq.forall (fun x -> innerMillerRabin num x s m d)
    if passesAllBases
    then
        Known proof
    else
        Known proof

let invalidCheck num =
    if num < 2I
    then Known(Invalid(num))
    else Unknown(num)

let smallPrimesCheck num =
    if Set.contains num smallPrimes
    then Known(Prime(num))
    else Unknown(num)

let smallPrimesDivisibleCheck num =
    if Set.exists (fun x -> bigint.Remainder(num, x) = 0I) smallPrimes
    then Known(Composite(num))
    else Unknown(num)

let fermatCheck num =
    let mode = bigint.ModPow(3I, num, num)
    if bigint.Compare(3I % num, mode) <> 0
    then Known(Composite(num))
    else Unknown(num)

let isPrime num =
    invalidCheck num
    |> PrimalityResult.bind smallPrimesCheck
    |> PrimalityResult.bind smallPrimesDivisibleCheck
    |> PrimalityResult.bind fermatCheck
    |> PrimalityResult.bind checkBases
