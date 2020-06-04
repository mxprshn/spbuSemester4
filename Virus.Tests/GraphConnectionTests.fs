module Virus.GraphConnectionTests

open NUnit.Framework
open FsUnit
open Virus

let testCases () =
    let windows98 = OS("Windows98", 40)
    let macOS = OS("MacOS", 20)
    let bolgenOS = OS("BolgenOS", 73)

    let computer0 = Computer(windows98, 0)
    let computer1 = Computer(windows98, 1)
    let computer2 = Computer(macOS, 2)
    let computer3 = Computer(bolgenOS, 3)

    [
        GraphConnection(array2D [[false; true; true; false]; [true; false; false; true]; [true; false; false; true]; [false; true; true; false]],
        [computer0; computer1; computer2; computer3]),
        [computer0; computer1; computer2; computer3],
        [[computer2; computer1]; [computer0; computer3]; [computer0; computer3]; [computer1; computer2]];

        GraphConnection(array2D [[false; false; false; false]; [false; false; false; false]; [false; false; false; false]; [false; false; false; false]],
        [computer0; computer1; computer2; computer3]),
        [computer0; computer1; computer2; computer3],
        [[]; []; []; []];

        GraphConnection(array2D [[true; true; true; true]; [true; true; true; true]; [true; true; true; true]; [true; true; true; true]],
        [computer0; computer1; computer2; computer3]),
        [computer0; computer1; computer2; computer3],
        [[computer1; computer2; computer3]; [computer0; computer2; computer3]; [computer0; computer1; computer3]; [computer0; computer1; computer2]];

        GraphConnection(array2D [[false; true; false; false]; [true; false; false; false]; [false; false; false; true]; [false; false; true; false]],
        [computer0; computer1; computer2; computer3]),
        [computer0; computer1; computer2; computer3],
        [[computer1]; [computer0]; [computer3]; [computer2]];

    ] |> List.map (fun (con, comp, n) -> TestCaseData(con, comp, n))

[<TestCaseSource("testCases")>]
let ``GetNeighboursTest``(connection: IConnection, computers: Computer List, expectedNeighbours: Computer List List) =
    expectedNeighbours |> List.iteri (fun i en -> 
        computers |> List.item i |> connection.GetNeighbours |> should equivalent en)

[<TestCaseSource("testCases")>]
let ``GetAllTest``(connection: IConnection, computers: Computer List, expectedNeighbours: Computer List List) =
    connection.GetAll() |> should equivalent computers