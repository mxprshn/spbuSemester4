module Homework2.Tests.ExpressionEvaluationTests

open NUnit.Framework
open FsUnit
open FsCheck
open ExpressionEvaluation
open System

[<Test>]
let ``Sum test`` () =
    Check.QuickThrowOnFailure (fun x y -> evaluate(Sum(Opnd(x), Opnd(y))) = (x + y))

[<Test>]
let ``Difference test`` () =
    Check.QuickThrowOnFailure (fun x y -> evaluate(Diff(Opnd(x), Opnd(y))) = (x - y))

[<Test>]
let ``Multiple test`` () =
    Check.QuickThrowOnFailure (fun x y -> evaluate(Mult(Opnd(x), Opnd(y))) |> should equal (x * y))

[<Test>]
let ``Quotient test with non zero divisor`` () =
    Check.QuickThrowOnFailure (fun x y ->
        match y with
        | 0 -> true
        | _ -> evaluate(Quot(Opnd(x), Opnd(y))) = (x / y))

[<Test>]
let ``Quotient test with zero divisor`` () =
    (fun () -> evaluate(Quot(Opnd(73), Opnd(0))) |> ignore) |> should throw typeof<DivideByZeroException>

[<Test>]
let ``Tree with all operators test`` () =
    Sum(Diff(Mult(Quot(Opnd(4), Opnd(2)), Quot(Opnd(4), Opnd(2))),
        Mult(Quot(Opnd(4), Opnd(2)), Quot(Opnd(4), Opnd(2)))),
        Diff(Mult(Quot(Opnd(4), Opnd(2)), Quot(Opnd(4), Opnd(2))),
        Mult(Quot(Opnd(4), Opnd(2)), Quot(Opnd(4), Opnd(2))))) |> evaluate |> should equal 0

let generateLongTree x =
    let rec generateLongTreeRecursive x = function
        | y when y = x -> Opnd(1)
        | y -> Sum(generateLongTreeRecursive x (y + 1), Opnd(1))
    generateLongTreeRecursive x 0

[<TestCase(10)>]
[<TestCase(100)>]
[<TestCase(1000)>]
let ``Long tree test`` x =
    generateLongTree x |> evaluate |> should equal (x + 1)