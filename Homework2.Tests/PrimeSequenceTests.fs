module Homework2.Tests.PrimeSequenceTests

open NUnit.Framework
open FsCheck
open PrimeSequence

let leavePrimes n =
    let rec leavePrimesRecursive list = function
        | i when i >= List.length list -> list
        | i -> leavePrimesRecursive (List.filter (fun x -> x % (List.item i list) <> 0) list) (i + 1)
    leavePrimesRecursive [1..n] 0

printf "%A" (leavePrimes 100)

//[<Test>]
//let ``Sequence contains prime numbers`` () = 
//    Check.QuickThrowOnFailure (x -> )

