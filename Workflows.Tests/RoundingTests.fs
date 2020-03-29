module Workflows.RoundingTests

open NUnit.Framework
open FsUnit
open FsCheck

open Rounding

type Overrides() =
    static member Float () =
        Arb.Default.Float () |> Arb.filter (fun f -> System.Double.IsNormal (f))
    static member Int32 () =
        Arb.Default.Int32 () |> Arb.filter (fun i -> i >= 0 && i <= 15)

[<SetUp>]
let ``SetUp``() =
    Arb.register<Overrides>() |> ignore
    
let rounding = RoundingBuilder

[<Test>]
let ``Return returns rounded numbers test`` () =
    Check.QuickThrowOnFailure (fun x prec ->
        let returned = rounding prec {
            return x
        }
        System.Math.Round (returned, prec) - returned = 0.0)

[<Test>]
let ``Bind returns rounded numbers test`` () =    
    Check.QuickThrowOnFailure (fun x prec ->
        let roundingPrec = rounding prec
        let result = roundingPrec.Bind (x, id)
        System.Math.Round (result, prec) - result = 0.0)

[<TestCase(0.0)>]
[<TestCase(1.0)>]
[<TestCase(0.737)>]
[<TestCase(-5.555)>]
[<TestCase(0.001)>]
[<TestCase(737373737373737373.737)>]
[<TestCase(System.Double.NaN)>]
[<TestCase(System.Double.PositiveInfinity)>]
[<TestCase(System.Double.NegativeInfinity)>]
let ``Bind and return are opposite test`` x =    
        let returned = rounding 3 {
            let! y = x 
            return y
        }
        returned |> should equal x

[<TestCase(-1)>]
[<TestCase(System.Int32.MinValue)>]
[<TestCase(System.Int32.MaxValue)>]
[<TestCase(16)>]
[<TestCase(73)>]
let ``Invalid precision value test`` prec =
    (fun () -> rounding prec |> ignore) |> should throw typeof<System.ArgumentException>