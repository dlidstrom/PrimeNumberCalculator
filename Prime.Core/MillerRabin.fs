namespace Prime.Core

open System.Linq
open System.Numerics
open Prime.Core.DomainTypes

module MillerRabin =
    let private smallPrimes =
        [  2I;  3I;  5I;  7I; 11I; 13I; 17I; 19I; 23I; 29I;
          31I; 37I; 41I; 43I; 47I; 53I; 59I; 61I; 67I; 71I;
          73I; 79I; 83I; 89I; 97I; 101I; 103I; 107I; 109I; 113I;
          127I; 131I; 137I; 139I; 149I; 151I; 157I; 163I; 167I; 173I;
          179I; 181I; 191I; 193I; 197I; 199I; 211I; 223I; 227I; 229I;
          233I; 239I; 241I; 251I; 257I ]
        |> Set.ofList

    let private fermat prim =
        let mode = BigInteger.ModPow(3I, prim, prim)
        if BigInteger.Compare(3I % prim, mode) <> 0
        then Primality.Composite
        else Primality.ProbablePrime

    (*let private innerMillerRabin prim a s m d =
        let firstModPow = BigInteger.ModPow(a, d, prim)
        if firstModPow = 1 || firstModPow = m
        then Primality.ProbablePrime
        else

        for (var j = 1; j < s; j++)
        {
            var exp = BigInteger.Pow(2, j) * d;
            var modPow = BigInteger.ModPow(a, exp, prim);
            if (modPow == m)
                return Primality.ProbablePrime;
        }

        return Primality.Composite;
    }

    let private checkBases prime =
        let getBases =
            function
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
        let bases = getBases prime

        let m = prime - 1I
        let mutable n = m
        let mutable s = 0
        while n.IsEven do
            s <- s + 1
            n <- BigInteger.Divide(n, 2I)

        match bases with
        | Some b ->
            let passesAllBases =
                bases
                |> List.forall (fun x -> InnerMillerRabin(prim, x, s, m, d) = Primality.ProbablePrime)
            if passesAllBases
            then Primality.Prime
            else Primality.Composite
        | _ ->
            Primality.Prime
*)
    let invalidCheck prim =
        if prim < 2I then Invalid else Unknown prim
    let smallPrimesCheck prim =
        if smallPrimes.Contains(prim) then Prime else Unknown prim
    let baseCheck prim =
        if smallPrimes.Any(fun x -> BigInteger.Remainder(prim, x) = 0I)
        then Composite
        else Unknown prim
    let check x =
        invalidCheck x
        |> Primality.bind smallPrimesCheck
        |> Primality.bind baseCheck

    function
        | prim when prim < 2I
            -> Primality.Composite
        | prim when smallPrimes.Contains(prim)
            -> Primality.Prime
        | prim when smallPrimes.Any(fun x -> BigInteger.Remainder(prim, x) = 0I) ->
            Primality.Composite
        | prim when fermat prim <> Primality.ProbablePrime ->
            Primality.Composite
        | _ -> Primality.Composite
            //| prim ->
            //    checkBases prim
(*

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