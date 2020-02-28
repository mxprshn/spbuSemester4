module Homework2.Tests.PrimeSequenceTests

open NUnit.Framework
open FsCheck
open FsUnit
open PrimeSequence

let leavePrimes n =
    let rec leavePrimesRecursive list =
        match list with
        | h::t -> h :: (leavePrimesRecursive <| List.filter (fun x -> x % h <> 0) t)
        | [] -> []
    leavePrimesRecursive [2..n]

[<Test>]
let ``Sequence contains prime numbers`` () =
    Check.QuickThrowOnFailure (fun x ->
        let expected = leavePrimes x
        let actual = Seq.take expected.Length primes |> Seq.toList
        actual |> should equal expected)