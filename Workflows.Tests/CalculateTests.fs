module Workflows.CalculateTests

open NUnit.Framework
open FsUnit
open FsCheck

open Calculate

type Overrides() =
    static member String () =
        Arb.Default.Int32 () |> Arb.convert (fun i -> i.ToString ()) System.Int32.Parse

let calculate = CalculateBuilder ()

let testCases =
    [
        "ololoOLOLOololo"
        "73.737373"
        "NaN"
        "\n"
        ""
    ] |> List.map (fun s -> TestCaseData s)

[<SetUp>]
let ``SetUp``() =
    Arb.register<Overrides>() |> ignore

[<Test>]
let ``Return test`` () =
    Check.QuickThrowOnFailure (fun s ->
        let returned = calculate {
            return s
        }
        System.Int32.Parse returned = s)

[<Test>]
let ``Bind test`` () =    
    Check.QuickThrowOnFailure (fun x ->
        let result = calculate.Bind (x, (fun i -> i.ToString ()))
        result = x)

[<Test>]
let ``Bind and return are opposite test`` () =    
    Check.QuickThrowOnFailure (fun x ->
        let returned = calculate {
            let! i = x 
            return i
        }
        returned = x)

[<TestCaseSource("testCases")>]
let ``Invalid string test`` s =
        let returned = calculate {
            let! i = s 
            return i
        }
        returned |> should equal "NaN"

[<TestCase("testCases")>]
let ``Invalid string Bind test`` s =    
        (s, (fun i -> i.ToString ())) |> calculate.Bind |> should equal "NaN"