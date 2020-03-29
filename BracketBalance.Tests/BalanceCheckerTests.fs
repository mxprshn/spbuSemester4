module BracketBalance.Tests

open NUnit.Framework
open FsUnit

open BalanceChecker

let testCases () =
    [
        "()", true
        "[]", true
        "{}", true
        "", true
        "olololo", true
        String.concat "ololo" <| [for i in 1..1000 -> "(o[l{o"]@[for i in 1..1000 -> "o}l]o)"], true
        String.concat "ololo" <| [for i in 1..100000 -> "("]@[for i in 1..100000 -> ")"], true
        String.concat " " <| [for i in 1..100000 -> "ololo"], true
        String.concat " " <| [for i in 1..100000 -> "()"], true
        String.concat "\n" <| [for i in 1..100000 -> "()"], true
        String.concat " " <| [for i in 1..100000 -> " "], true
        "(", false
        "[", false
        "{", false
        ")", false
        "]", false
        "}", false
        "(]", false
        "(]", false
        "[)", false
        "[}", false
        "{)", false
        "{]", false
        String.concat "." <| "["::[for i in 1..1000 -> "("]@[for i in 1..1000 -> ")"], false
        String.concat "" <| [for i in 1..1000 -> "["]@("("::[for i in 1..1000 -> "]"]), false        
    ] |> List.map (fun (s, e) -> TestCaseData(s, e))

[<TestCaseSource("testCases")>]
let ``Check balance test`` str exp =
    str |> checkBalance |> should equal exp