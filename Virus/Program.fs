open Virus

[<EntryPoint>]
let main argv =

    let windows98 = OS("Windows98", 10)
    let macOS = OS("MacOS", 25)
    let bolgenOS = OS("BolgenOS", 50)

    let computer0 = Computer(windows98, 0)
    let computer1 = Computer(windows98, 1)
    let computer2 = Computer(macOS, 2)
    let computer3 = Computer(bolgenOS, 3)

    computer2.Contaminate();

    let connection = GraphConnection(array2D [[false; true; true; false]; [true; false; false; true]; [true; false; false; true]; [false; true; true; false]],
    [computer0; computer1; computer2; computer3])

    let network = Network(connection)

    network.PrintState()

    let rec move counter =
        if (counter < 10) then
            network.MakeMove()
            network.PrintState()
            move (counter + 1)
    move 0

    0