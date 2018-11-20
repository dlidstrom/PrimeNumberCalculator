namespace Prime.Core

(*
public static class BigIntegerExtensions
{
    private static readonly RNGCryptoServiceProvider Rng = new RNGCryptoServiceProvider();

    public static BigInteger Sqrt(this BigInteger n)
    {
        if (n == 0) return 0;
        if (n <= 0) throw new ArithmeticException("NaN");

        var bitLength = Convert.ToInt32(Math.Ceiling(BigInteger.Log(n, 2)));
        var root = BigInteger.One << (bitLength / 2);

        while (!IsSqrt(n, root))
        {
            root += n / root;
            root >>= 1;
        }

        return root;
    }

    public static BigInteger RandomIntegerBelow(BigInteger bound)
    {
        // Get a byte buffer capable of holding any value below the bound
        var buffer = (bound << 16).ToByteArray(); // << 16 adds two bytes, which decrease the chance of a retry later on

        // Compute where the last partial fragment starts, in order to retry if we end up in it
        var generatedValueBound = BigInteger.One << (buffer.Length * 8 - 1); // -1 accounts for the sign bit
        var validityBound = generatedValueBound - generatedValueBound % bound;

        while (true)
        {
            // generate a uniformly random value in [0, 2^(buffer.Length * 8 - 1))
            Rng.GetBytes(buffer);
            buffer[buffer.Length - 1] &= 0x7F; // force sign bit to positive
            var r = new BigInteger(buffer);

            // return unless in the partial fragment
            if (r >= validityBound) continue;
            return r % bound;
        }
    }

    public static BigInteger Gcd(this BigInteger x, BigInteger y)
    {
        while (true)
        {
            if (x == 0)
                return y;
            if (y == 0)
                return x;

            x = x%y;
            var x1 = x;
            x = y;
            y = x1;
        }
    }

    private static Boolean IsSqrt(BigInteger n, BigInteger root)
    {
        var lowerBound = root * root;
        var upperBound = (root + 1) * (root + 1);

        return n >= lowerBound && n < upperBound;
    }
}
*)
