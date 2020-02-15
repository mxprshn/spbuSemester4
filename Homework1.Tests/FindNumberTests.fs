module Homework1.Tests.FindNumber

open NUnit.Framework
open FindNumber

let testCases =
    [
        [1; 2; 3; 4; 5], 1, 0
        [73], 73, 0
        [1; 1; 1; 1; 1; 1], 1, 0
        [0; -1; -100; -1000; -100; -1; 0], -100, 2
    ] |> List.map (fun (l, n, e) -> TestCaseData(l, n, e))

let invalidTestCases =
    [
        [], 73
        [73], 1
        [1; 1; 1; 1; 1; 1], 0
    ] |> List.map (fun (l, n) -> TestCaseData(l, n))

[<TestCaseSource("testCases")>]
[<Test>]
let findNumberTest list n expected =
    Assert.AreEqual(Some(expected), findNumber list n 0)

[<TestCaseSource("invalidTestCases")>]
[<Test>]
let findNumberInvalidTest list n =
    Assert.AreEqual(None, findNumber list n 0)