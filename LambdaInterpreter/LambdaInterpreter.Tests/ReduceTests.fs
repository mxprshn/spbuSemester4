module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open Interpreter

let testCases =
    [
        App(Abs('x', Var('x')), Var('y')), Var('y')
        App(Abs('x', Var('y')), App(Abs('x', App(App(Var('x'), Var('x')), Var('x'))), Abs('x', App(App(Var('x'), Var('x')), Var('x'))))), Var('y')
        Var('x'), Var('x')
        App(Var('x'), Var('y')), App(Var('x'), Var('y'))
        Abs('x', App(Abs('y', Var('y')), Var('x'))), Abs('x', Var('x'))
        App(Abs('x', Abs('y', Var('x'))), Var('y')), Abs('a', Var('y'))
        App(App(Abs('x', Var('a')), Var('x')), App(Abs('y', Var('b')), Var('y'))), App(Var('a'), Var('b'))
        App(App(Abs('x', Abs('y', Abs('z', App(App(Var('x'), Var('z')), App(Var('y'), Var('z')))))), Abs('x', Abs('y', Var('x')))), Abs('x', Abs('y', Var('x')))), Abs('z', Var('z'))

    ] |> List.map (fun (l, e) -> TestCaseData(l, e))

[<TestCaseSource("testCases")>]
let reduceTest l e =
    l |> reduce |> should equal e
    
