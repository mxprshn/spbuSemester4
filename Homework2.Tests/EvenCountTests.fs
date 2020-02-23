module Homework2.Tests.EvenCountTests

open NUnit.Framework
open FsUnit
open FsCheck
open EvenCount

let testCases =
    [
        [], 0
        [2], 1
        [1], 0
        [1; 2; 3; 4; 5], 2
        [0; 2; 4; 8; 16; 32; 64], 7
        [-1; -2; -3; -4; -5], 2
        [-2; 2; -4; 4; -8; 8], 6
        [for x in 1..100000 -> 2], 100000
        [for x in 1..100000 -> x % 2], 50000
    ] |> List.map (fun (l, e) -> TestCaseData(l, e))

[<TestCaseSource("testCases")>]
let ``Count even elements of list with evenCountFilter`` list expected =
    evenCountFilter list |> should equal expected

[<TestCaseSource("testCases")>]
let ``Count even elements of list with evenCountFold`` list expected =
    evenCountFold list |> should equal expected

[<TestCaseSource("testCases")>]
let ``Count even elements of list with evenCountMap`` list expected =
    evenCountMap list |> should equal expected

[<Test>]
let ``Functions return equal results`` = 
    Check.QuickThrowOnFailure (fun x -> evenCountFold x = evenCountFilter x && evenCountFold x = evenCountMap x)