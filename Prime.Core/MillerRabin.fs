namespace Prime.Core

module MillerRabin =
    open Prime.Core.DomainTypes
    let isPrime =
        function
            | prim when prim < 2 -> Primality.Composite
            | _ -> Primality.Prime
        //if (SmallPrimes.Contains(prim)) return Primality.Prime;
        //if (SmallPrimes.Any(smallPrime => BigInteger.Remainder(prim, smallPrime) == 0)) return Primality.Composite;
        //if (Fermat(prim) != Primality.ProbablePrime) return Primality.Composite;

(*
public class PrimeCalculator
{
    private static readonly HashSet<BigInteger> SmallPrimes = new HashSet<BigInteger>
    {
        2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
        31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
        73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
        127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
        179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
        233, 239, 241, 251, 257
    };

    public Primality MillerRabin(BigInteger prim)
    {
        // quick checks
        if (prim < 2) return Primality.Composite;
        if (SmallPrimes.Contains(prim)) return Primality.Prime;
        if (SmallPrimes.Any(smallPrime => BigInteger.Remainder(prim, smallPrime) == 0)) return Primality.Composite;
        if (Fermat(prim) != Primality.ProbablePrime) return Primality.Composite;

        int[] bases = null;
        if (prim < 1373653) bases = new[] { 2, 3 };
        else if (prim < 9080191) bases = new[] { 31, 73 };
        else if (prim < 25326001) bases = new[] { 2, 3, 5 };
        else if (prim < 4759123141) bases = new[] { 2, 7, 61 };
        else if (prim < 1122004669633) bases = new[] { 2, 13, 23, 1662803 };
        else if (prim < 2152302898747) bases = new[] { 2, 3, 5, 7, 11 };
        else if (prim < 3474749660383) bases = new[] { 2, 3, 5, 7, 11, 13 };
        else if (prim < 341550071728321) bases = new[] { 2, 3, 5, 7, 11, 13, 17 };
        else if (prim < 3825123056546413051) bases = new[] { 2, 3, 5, 7, 11, 13, 17, 19, 23 };

        var m = prim - 1;
        var n = m;
        var s = 0;
        while (n.IsEven)
        {
            s++;
            n >>= 1;
        }

        var d = m / BigInteger.Pow(2, s);

        if (bases != null)
        {
            var passesAllBases = bases.All(x => InnerMillerRabin(prim, x, s, m, d) == Primality.ProbablePrime);
            return passesAllBases ? Primality.Prime : Primality.Composite;
        }

        // try random bases
        for (var i = 0; i < 20; i++)
        {
            var based = BigIntegerExtensions.RandomIntegerBelow(prim - 2);
            if (InnerMillerRabin(prim, based, s, m, d) != Primality.ProbablePrime)
            {
                return Primality.Composite;
            }
        }

        return Primality.ProbablePrime;
    }

    public BigInteger[] Factorize(BigInteger prim, CancellationToken token)
    {
        if (prim < 1) throw new ArgumentException("Must be at least 1", "prim");
        if (prim == 1 || prim == 2 || MillerRabin(prim) != Primality.Composite) return new[] { prim };
        if (prim.IsEven)
        {
            var two = new List<BigInteger> { 2 };
            two.AddRange(Factorize(prim >> 1, token));
            return two.OrderBy(x => x).ToArray();
        }

        var factors = new List<BigInteger>();
        var stack = new Stack<BigInteger>();
        stack.Push(prim);
        while (stack.Count > 0)
        {
            var n = stack.Pop();
            if (n.IsEven)
            {
                factors.Add(2);
                stack.Push(n / 2);
            }
            else if (MillerRabin(n) != Primality.Composite)
            {
                factors.Add(n);
            }
            else
            {
                //var factor = FermatFactor(n, token);
                var factor = PollardRho(n, token);
                var f = n / factor;
                if (MillerRabin(factor) != Primality.Composite)
                {
                    factors.Add(factor);
                }
                else
                {
                    stack.Push(n / f);
                }

                stack.Push(f);
            }
        }

        return factors.OrderBy(x => x).ToArray();
    }

    public BigInteger NextBigger(BigInteger num)
    {
        var nextBigger = num.IsEven ? num + 1 : num + 2;
        while (MillerRabin(nextBigger) == Primality.Composite)
        {
            nextBigger += 2;
        }

        return nextBigger;
    }

    public BigInteger PreviousSmaller(BigInteger prim)
    {
        var previousSmaller = prim.IsEven ? prim - 1 : prim - 2;
        if (previousSmaller < 2) previousSmaller = 1;
        while (previousSmaller > 1 && MillerRabin(previousSmaller) == Primality.Composite)
        {
            previousSmaller -= 2;
        }

        return previousSmaller;
    }

    private static BigInteger PollardRho(BigInteger number, CancellationToken token)
    {
        var c = 1;
        while (true)
        {
            var currentC = c;
            Func<BigInteger, BigInteger, BigInteger> func = (x, mod) => (x * x + currentC) % mod;
            var result = TryPollardRho(number, func, token);
            if (result > 0) return result;
            c += 2;
        }
    }

    private static BigInteger TryPollardRho(BigInteger number, Func<BigInteger, BigInteger, BigInteger> func, CancellationToken token)
    {
        BigInteger a = 2;
        BigInteger b = 2;
        while (true)
        {
            token.ThrowIfCancellationRequested();

            a = func(a, number);
            b = func(func(b, number), number);
            var diff = BigInteger.Abs(b - a);
            var tmp = diff.Gcd(number);

            if (tmp == number)
                return -1;

            if (tmp > 1)
                return tmp;
        }
    }

    private static Primality InnerMillerRabin(BigInteger prim, BigInteger a, int s, BigInteger m, BigInteger d)
    {
        var firstModPow = BigInteger.ModPow(a, d, prim);
        if (firstModPow == 1 || firstModPow == m)
            return Primality.ProbablePrime;

        for (var j = 1; j < s; j++)
        {
            var exp = BigInteger.Pow(2, j) * d;
            var modPow = BigInteger.ModPow(a, exp, prim);
            if (modPow == m)
                return Primality.ProbablePrime;
        }

        return Primality.Composite;
    }

    private static Primality Fermat(BigInteger prim)
    {
        var mode = BigInteger.ModPow(3, prim, prim);
        return 3 % prim != mode ? Primality.Composite : Primality.ProbablePrime;
    }

    private static BigInteger FermatFactor(BigInteger oddNumber, CancellationToken token)
    {
        if (oddNumber.IsEven) throw new ArgumentException("Must be odd number", "oddNumber");
        if (oddNumber < 3) throw new ArgumentException("Must be 3 or greater", "oddNumber");
        var a = oddNumber.Sqrt();
        if (a * a != oddNumber)
            a++;
        var b2 = a * a - oddNumber;

        var tmp = b2.Sqrt();
        while (tmp * tmp != b2)
        {
            token.ThrowIfCancellationRequested();

            a += 1;
            b2 = a * a - oddNumber;
            tmp = b2.Sqrt();
        }

        return a + tmp;
    }
}
*)
