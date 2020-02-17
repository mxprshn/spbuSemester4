module Homework1.Tests.Reverse

open NUnit.Framework
open Reverse

let testCases =
    [
        [], []
        [1; 2; 3; 4; 5], [5; 4; 3; 2; 1]
        [73], [73]
        [1; 1; 1; 1; 1; 1], [1; 1; 1; 1; 1; 1]
        [0; -1; -100; -1000; -100; -1; 0], [0; -1; -100; -1000; -100; -1; 0]
    ] |> List.map (fun (l, e) -> TestCaseData(l, e))

[<TestCaseSource("testCases")>]
[<Test>]
let reverseTest list expected =
    Assert.AreEqual(expected, reverse list [])