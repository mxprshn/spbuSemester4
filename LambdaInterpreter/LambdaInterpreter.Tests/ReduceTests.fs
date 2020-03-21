module LambdaInterpreter.Tests

open NUnit.Framework
open Interpreter

let testCases =
    [
        App(Abs('x', Var('x')), Var('y')), Var('y') 
    ] |> List.map (fun (l, e) -> TestCaseData(l, e))

[<TestCaseSource("testCases")>]
let reduceTest  =
    Assert.Pass()
