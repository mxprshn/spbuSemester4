module Homework2.Tests.TreeMapTests

open NUnit.Framework
open FsUnit
open TreeMap

let intTestCases =
    [
        Node(0, Node(1, Empty, Node(4, Empty, Empty)), Node(2, Empty, Empty)),
        (fun x -> x + 1),
        Node(1, Node(2, Empty, Node(5, Empty, Empty)), Node(3, Empty, Empty))

        Node(0, Node(1, Empty, Node(4, Empty, Empty)), Node(2, Empty, Empty)),
        (fun x -> 0),
        Node(0, Node(0, Empty, Node(0, Empty, Empty)), Node(0, Empty, Empty))

        Node(0, Node(1, Empty, Node(4, Empty, Empty)), Node(2, Empty, Empty)),
        id,
        Node(0, Node(1, Empty, Node(4, Empty, Empty)), Node(2, Empty, Empty))

        Empty,
        (fun x -> x + 1),
        Empty

        Node(0, Empty, Node(1, Empty, Node(2, Empty, Node(3, Empty, Node(4, Empty,
            Node(5, Empty, Node(6, Empty, Node(7, Empty, Node(8, Empty, Node(9, Empty, Node(10, Empty, Empty))))))))))),
        id,
        Node(0, Empty, Node(1, Empty, Node(2, Empty, Node(3, Empty, Node(4, Empty,
            Node(5, Empty, Node(6, Empty, Node(7, Empty, Node(8, Empty, Node(9, Empty, Node(10, Empty, Empty)))))))))))
    ] |> List.map (fun (t, f, e) -> TestCaseData(t, f, e))

let stringTestCases =
    [
        Node("ololo", Empty, Empty),
        String.length,
        Node(5, Empty, Empty)
    ] |> List.map (fun (t, f, e) -> TestCaseData(t, f, e))

let listTestCases =
    [
        Node([1; 2; 3], Empty, Empty),
        List.map (fun x -> x * 2),
        Node([2; 4; 6], Empty, Empty)

        Node([1; 2; 3; 4; 5], Empty, Empty),
        List.filter (fun x -> x % 2 = 0),
        Node([2; 4], Empty, Empty)
    ] |> List.map (fun (t, f, e) -> TestCaseData(t, f, e))

let rec treesEqual tree1 tree2 =
    match (tree1, tree2) with
    | (Node(x, l1, r1), Node(y, l2, r2)) when x = y ->
        treesEqual l1 l2 && treesEqual r1 r2
    | (Empty, Empty) -> true
    | _ -> false

[<TestCaseSource("listTestCases")>]
[<TestCaseSource("intTestCases")>]
[<TestCaseSource("stringTestCases")>]
let ``Tree map test`` tree f expected =
    treesEqual (treeMap f tree) expected |> should be True