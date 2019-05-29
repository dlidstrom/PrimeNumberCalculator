module BigIntegerExtensions

open System
open System.Security.Cryptography

let rng = new RNGCryptoServiceProvider()

let randomIntegerBelow (bound : bigint) =
    // Get a byte buffer capable of holding any value below the bound
    // << 16 adds two bytes, which decrease the chance of a retry later on
    let buffer = bigint.Multiply(bound, bigint.Pow(2I, 16)).ToByteArray();

    // Compute where the last partial fragment starts, in order to retry if we end up in it
    // -1 accounts for the sign bit
    let generatedValueBound = bigint.Pow(2I, buffer.Length * 8 - 1)
    let validityBound = generatedValueBound - generatedValueBound % bound;

    let rec uniformRandom (buffer : byte[]) =
        rng.GetBytes(buffer)
        buffer.[buffer.Length - 1] <- buffer.[buffer.Length - 1] &&& 127uy
        let r = bigint(buffer)

        // return unless in the partial fragment
        if r >= validityBound
        then uniformRandom buffer
        else r % bound

    uniformRandom buffer

let ten = 10I  // 10^1
let tenBillion = 10000000000I // 10^10
let googol = 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000I // 10^100
let googolToTenth = googol * googol * googol * googol * googol * googol * googol * googol * googol * googol // 10^10000

let private power10 (exponent : bigint) =
    if exponent.Sign = -1 then
        0I
    else
        let rec expox (product : bigint) (pow : bigint) multiplier log10OfMult =
            match pow with
            | x when x < log10OfMult -> product, pow
            | _ -> expox (product * multiplier) (pow - log10OfMult) multiplier log10OfMult
        let pow10To1000, rem1 = expox 1I exponent googolToTenth 1000I
        let pow10To100, rem2 = expox 1I rem1 googol 100I
        let pow10To10, rem3 = expox 1I rem2 tenBillion 10I
        pow10To1000 * pow10To100 * pow10To10 * fst(expox 1I rem3 10I 1I)

let private log10 theNumber =
    if theNumber <= 0I then
        failwith "Logarithm of a non-positive number is not defined."
    // this inner functions counts the number of times divisor will divide num
    let rec divisions count (num : bigint) divisor =
        match num with
        | x when x < divisor -> count, num
        | _ -> divisions (count + 1I) (num / divisor) divisor
    let thousandsCount, rem1 = divisions 0I theNumber googolToTenth
    let hundredsCount, rem2 = divisions 0I rem1 googol
    let tensCount, rem3 = divisions 0I rem2 tenBillion
    1000I * thousandsCount + 100I * hundredsCount + ten * tensCount + fst(divisions 0I rem3 10I)

let sqrtRoughGuess (num : bigint) =
    let log10x = log10 (num + 1I)
    let halfLog = (log10x + 1I) >>> 1
    (power10 halfLog)

let bigintSqrt (bigNum : bigint) =
    let rec converge prevGuess =
        let nextGuess = (bigNum / prevGuess + prevGuess) >>> 1
        match bigint.Abs (prevGuess - nextGuess) with
        | x when x < 2I -> nextGuess
        | _ -> converge nextGuess
    if bigNum.Sign = -1 then
        failwith "Square root of a negative number is not defined."
    else
        let root = converge (sqrtRoughGuess bigNum)
        if root * root > bigNum then
            root - 1I
        else
            root

let sqrt (n : bigint) =
    let isSqrt n root =
        let lowerBound = root * root
        let upperBound = (root + 1I) * (root + 1I)
        n >= lowerBound && n < upperBound

    match n with
    | n when n.IsZero -> 0I
    | n when n <= 0I -> raise (ArithmeticException("NaN"))
    | n ->
        let rec loop root =
            if isSqrt n root
            then root
            else
                let nextRoot = (root + n / root) >>> 1
                loop nextRoot

        let bitLength = Convert.ToInt32(Math.Ceiling(bigint.Log(n, 2.)))
        let root = 1I <<< (bitLength / 2)
        loop root
