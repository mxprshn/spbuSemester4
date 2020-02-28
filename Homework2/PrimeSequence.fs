module PrimeSequence

let primes =
    let isPrime n =
        let rec isPrimeRecursive n = function
            | i when i > (n |> float |> sqrt |> round |> int) -> true
            | i when n % i = 0 -> false
            | i -> isPrimeRecursive n (i + 1)
        isPrimeRecursive n 2
    Seq.initInfinite (fun x -> x + 2) |> Seq.filter isPrime

printf "%A" (Seq.take 10000 primes |> Seq.toList)
    

