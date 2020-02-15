module Homework1.Tests.Factorial

open NUnit.Framework
open Factorial

[<TestCase(0, 1, 1, 1)>]
[<TestCase(1, 1, 1, 1)>]
[<TestCase(2, 1, 1, 2)>]
[<TestCase(3, 1, 1, 6)>]
[<TestCase(4, 1, 1, 24)>]
[<TestCase(8, 1, 1, 40320)>]
[<TestCase(10, 1, 1, 3628800)>]
[<Test>]
let factorialTest x acc i expected =
    Assert.AreEqual(Some(expected), factorial x acc i)

[<TestCase(-100, 1, 1)>]
[<TestCase(-73, 1, 1)>]
[<Test>]
let factorialInvalidTest x acc i =
    Assert.AreEqual(None, factorial x acc i)