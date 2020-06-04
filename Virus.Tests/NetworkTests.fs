module Virus.NetworkTests

open NUnit.Framework
open FsUnit
open Virus

let testCases () =
    let bolgenOSUltra = OS("BolgenOSUltra", 100)
    let pureLinux = OS("PureLinux", 0)

    let computer0 = Computer(bolgenOSUltra, 0)
    let computer1 = Computer(bolgenOSUltra, 1)
    let computer2 = Computer(bolgenOSUltra, 2)
    let computer3 = Computer(bolgenOSUltra, 3)
    let computer4 = Computer(pureLinux, 4)
    let computer5 = Computer(pureLinux, 5)
    let computer6 = Computer(pureLinux, 6)
    let computer7 = Computer(pureLinux, 7)

    [
        GraphConnection(array2D [[false; true; false; false]; [true; false; true; false]; [false; true; false; true]; [false; false; true; false]],
        [computer0; computer1; computer2; computer3]),
        computer0,
        [[computer0]; [computer0; computer1]; [computer0; computer1; computer2]; [computer0; computer1; computer2; computer3]];

        GraphConnection(array2D [[false; true; false; false]; [true; false; true; false]; [false; true; false; true]; [false; false; true; false]],
        [computer0; computer1; computer2; computer3]),
        computer3,
        [[computer3]; [computer3; computer2]; [computer3; computer2; computer1]; [computer3; computer2; computer1; computer0]];

        GraphConnection(array2D [[false; true; false; false]; [true; false; true; false]; [false; true; false; true]; [false; false; true; false]],
        [computer0; computer1; computer2; computer3]),
        computer1,
        [[computer1]; [computer0; computer1; computer2]; [computer0; computer1; computer2; computer3]];

        GraphConnection(array2D [[false; true; false; false]; [true; false; true; false]; [false; true; false; false]; [false; false; false; false]],
        [computer0; computer1; computer2; computer3]),
        computer0,
        [[computer0]; [computer0; computer1]; [computer0; computer1; computer2]; [computer0; computer1; computer2]; [computer0; computer1; computer2]];

        GraphConnection(array2D [[false; true; false; false]; [true; false; true; false]; [false; true; false; false]; [false; false; false; false]],
        [computer0; computer1; computer2; computer3]),
        computer3,
        [[computer3]; [computer3]; [computer3]; [computer3]];

        GraphConnection(array2D [[false; true; false; false]; [true; false; true; false]; [false; true; false; true]; [false; false; true; false]],
        [computer0; computer1; computer4; computer3]),
        computer0,
        [[computer0]; [computer0; computer1]; [computer0; computer1]; [computer0; computer1]; [computer0; computer1]];

        GraphConnection(array2D [[false; true; false; false]; [true; false; true; false]; [false; true; false; true]; [false; false; true; false]],
        [computer0; computer1; computer4; computer3]),
        computer3,
        [[computer3]; [computer3]; [computer3]; [computer3]];

        GraphConnection(array2D [[true; true; true; true]; [true; true; true; true]; [true; true; true; true]; [true; true; true; true]],
        [computer4; computer5; computer6; computer7]),
        computer4,
        [[computer4]; [computer4]; [computer4]; [computer4]];

    ] |> List.map (fun (con, i, m) -> TestCaseData(con, i, m))

[<TestCaseSource("testCases")>]
let ``Contamination test``(connection: IConnection, initial: Computer, states: Computer List List) =
    connection.GetAll() |> Seq.iter (fun c -> c.Heal())
    let network = Network(connection)
    initial.Contaminate()

    let rec makeMove (currentStates: Computer List List) =
        match currentStates with
        | h::t ->
            let contaminated = network.Connection.GetAll() |> Seq.filter (fun c -> c.IsContaminated)
            contaminated |> should equivalent h
            network.MakeMove()
            makeMove t
        | _ -> ()

    makeMove states

