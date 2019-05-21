module BigIntegerExtensions

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