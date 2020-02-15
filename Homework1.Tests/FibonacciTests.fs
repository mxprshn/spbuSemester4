module Homework1.Tests.Fibonacci

open NUnit.Framework
open Fibonacci

[<TestCase(0, 0, 1, 0, 0)>]
[<TestCase(1, 0, 1, 0, 1)>]
[<TestCase(2, 0, 1, 0, 1)>]
[<TestCase(3, 0, 1, 0, 2)>]
[<TestCase(4, 0, 1, 0, 3)>]
[<TestCase(5, 0, 1, 0, 5)>]
[<TestCase(6, 0, 1, 0, 8)>]
[<TestCase(7, 0, 1, 0, 13)>]
[<TestCase(10, 0, 1, 0, 55)>]
[<TestCase(31, 0, 1, 0, 1346269)>]
[<Test>]
let fibonacciTest x acc1 acc2 i expected =
    Assert.AreEqual(Some(expected), fibonacci x acc1 acc2 i)

[<TestCase(-1, 0, 1, 0)>]
[<TestCase(-200, 0, 1, 0)>]
[<Test>]
let fibonacciInvalidTest x acc1 acc2 i =
    Assert.AreEqual(None, fibonacci x acc1 acc2 i)