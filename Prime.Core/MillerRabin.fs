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

let private innerMillerRabin num a s m d =
    let firstModPow = bigint.ModPow(a, d, num)
    if firstModPow = 1I || firstModPow = m
    then
        ProbablePrime num
    else
        let rec tryPow j =
            if j < s
            then
                let exp = bigint.Pow(2I, j) * d
                let modPow = bigint.ModPow(a, exp, num)
                if modPow = m
                then
                    ProbablePrime num
                else
                    tryPow (j + 1)
            else
                Composite num

        tryPow 1

let checkBases num =
    // todo: change into array only
    let bases =
        match num with
        | prim when prim < 1373653I ->
            Some [ 2I; 3I ]
        | prim when prim < 9080191I ->
            Some [ 31I; 73I ]
        | prim when prim < 25326001I ->
            Some [ 2I; 3I; 5I ]
        | prim when prim < 4759123141I ->
            Some [ 2I; 7I; 61I ]
        | prim when prim < 1122004669633I ->
            Some [ 2I; 13I; 23I; 1662803I ]
        | prim when prim < 2152302898747I ->
            Some [ 2I; 3I; 5I; 7I; 11I ]
        | prim when prim < 3474749660383I ->
            Some [ 2I; 3I; 5I; 7I; 11I; 13I ]
        | prim when prim < 341550071728321I ->
            Some [ 2I; 3I; 5I; 7I; 11I; 13I; 17I ]
        | prim when prim < 3825123056546413051I ->
            Some [ 2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I ]
        | _ -> None

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
    let result =
        match bases with
        | Some b ->
            let passesAllBases =
                b
                |> List.forall (fun x ->
                    match innerMillerRabin num x s m d with
                    | ProbablePrime _ -> true
                    | _ -> false)
            if passesAllBases
            then
                Prime num
            else
                Composite num
        | _ ->
            // try random bases
            let rec tryRandomBase (i : int) =
                let result =
                    if i < 20
                    then
                        let based = BigIntegerExtensions.randomIntegerBelow (num - 2I)
                        let (a : Classification) =
                            match innerMillerRabin num based s m d with
                            | ProbablePrime _ ->
                                let result1 = tryRandomBase (i + 1)
                                result1
                            | _ ->
                                Composite num
                        a
                    else
                        ProbablePrime num
                result

            let result = tryRandomBase 0
            result
    Known result

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
