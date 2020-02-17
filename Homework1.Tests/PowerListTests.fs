module Homework1.Tests.PowerList

open NUnit.Framework
open PowerList

let testCases =
    [
        1, 4, [2; 4; 8; 16; 32]
        0, 4, [1; 2; 4; 8; 16]
        10, 1, [1024; 2048]
        1, 1, [2; 4]
        0, 0, [1]
        3, 0, [8]
    ] |> List.map (fun (n, m, e) -> TestCaseData(n, m, e))

[<TestCaseSource("testCases")>]
[<Test>]
let powerListTest n m expected =
    Assert.AreEqual(Some(expected), powerList n m)

let powerListInvalidTest () =
    Assert.AreEqual(None, powerList -10 2)