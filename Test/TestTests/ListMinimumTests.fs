module TestsTests.ListMinimumTests

open Minimum
open NUnit.Framework
open FsCheck
open FsUnit

let testCases =
    [
        [for x in 1..100000 -> 100000 - x], Some(0)
        [for x in 1..100000 -> x % 2], Some(0)
    ] |> List.map (fun (l, e) -> TestCaseData(l, e))

[<Test>]
let ``Custom and library minimum return the same`` () = 
    Check.QuickThrowOnFailure (fun l ->
        if not (List.length l = 0) then
            Some(List.min l) = minimum l
        else true)

[<TestCaseSource("testCases")>]
let ``Long list test`` list exp =
    list |> minimum |> should equal exp

[<Test>]
let ``Empty list test`` () =
    [] |> minimum |> should equal None
