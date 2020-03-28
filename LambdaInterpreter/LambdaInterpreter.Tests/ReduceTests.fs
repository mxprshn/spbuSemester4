module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open Interpreter

let True = Abs('x', Abs('y', Var 'x'))
let False = Abs('x', Abs('y', Var 'y'))
let If = Abs('b', Abs('t', Abs('f', App(App(Var 'b', Var 't'), Var 'f'))))
let And = Abs('a', Abs('b', App(App(App(If, Var 'a'), Var 'b'), False)))
let Or = Abs('a', Abs('b', App(App(App(If, Var 'a'), True), Var 'b')))
let Not = Abs('b', App(App(App(If, Var 'b'), False), True))

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
        App(App(Var('x'), Var('y')), App(Var 'a', Var 'b')), App(App(Var('x'), Var('y')), App(Var 'a', Var 'b'))
        App(App(And, True), False), False
        App(App(And, True), True), True
        App(App(And, False), False), False
        App(App(And, False), True), False
        App(App(Or, True), False), True
        App(App(Or, True), True), True
        App(App(Or, False), False), False
        App(App(Or, False), True), True 
        App(Not, True), False
        App(Not, False), True
    ] |> List.map (fun (l, e) -> TestCaseData(l, e))

[<TestCaseSource("testCases")>]
let reduceTest l e =
    l |> reduce |> should equal e


    
