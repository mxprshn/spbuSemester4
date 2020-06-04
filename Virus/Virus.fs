module Virus

// Class representing an operating system.
type OS(name: string, contagionProbability: int) =

    do
        if (contagionProbability < 0 || contagionProbability > 100) then
            invalidArg "contagionProbability" "Contagion probability must be a number in range [0; 100]"

    // Name of the OS.
    member this.Name = name

    // Virus contagion probability in percents. 
    member this.ContagionProbability = contagionProbability

// Class representing a computer with installed operating system.
type Computer(os: OS, id: int) =

    let mutable isContaminated = false

    // Installed OS.
    member this.OS = os

    // Identifier.
    member this.Id = id

    // Shows if the computer is contaminated with virus.
    member this.IsContaminated
        with get() = isContaminated

    // Contaminates computer.
    member this.Contaminate() =
        isContaminated <- true

    // Returnes computer to clean state. 
    member this.Heal() =
        isContaminated <- false

    override this.Equals(obj: obj) = 
        let other = obj :?> Computer
        other.Id = id && other.OS = os

    override this.GetHashCode() =
        id

// Interface of connection between multiple computers.
type IConnection =

    // Returns list of computers connected to the specified computer.
    abstract member GetNeighbours: Computer -> Computer List

    // Returnes list of all connected computers.
    abstract member GetAll: unit -> Computer List

// Class representing computer connection defined with adjacency matrix.
type GraphConnection(adjacencyMatrix: bool[,], computers: Computer List) =

    do
        if (adjacencyMatrix.GetLength 0 <> adjacencyMatrix.GetLength 1) then
            invalidArg "adjacencyMatrix" "Adjacency matrix must be a square matrix"
        if (Seq.length computers <> adjacencyMatrix.GetLength 0) then
            invalidArg "computers" "Number of computers must be equal to the dimension of adjacencyMartix"

    interface IConnection with

        member this.GetNeighbours computer =
            let index = computers |> List.findIndex (fun c -> c.Id = computer.Id)
            let neighbourIndexes = adjacencyMatrix.[index, *] |> Seq.toList|>
                List.indexed |>
                List.filter (fun (i, n) -> n && i <> index) |>
                List.map (fun (i, n) -> i)
            computers |>
                List.indexed |>
                List.filter (fun (i, c) -> Seq.contains i neighbourIndexes) |>
                List.map (fun (i, c) -> c)

        member this.GetAll() =
            computers

// Class representing computer network with viruses.
type Network(connection: IConnection) =

    let randomGenerator = System.Random()

    let shouldBeContaminated (computer: Computer) =
        computer.OS.ContagionProbability >= randomGenerator.Next(1, 100)
        
    let makeMove (computers: Computer List) =
        let toContaminate = computers |>
            List.filter (fun c -> c.IsContaminated) |>
            List.collect connection.GetNeighbours |>
            List.filter shouldBeContaminated
        let rec contaminateRecursive (current: Computer List) =
            match current with 
            | h::t -> 
                h.Contaminate()
                contaminateRecursive t
            | _ -> ()
        contaminateRecursive toContaminate

    // Prints contagion state of all computers.
    member this.PrintState() =
        connection.GetAll() |> List.iter (fun c -> 
            let state = match c.IsContaminated with
            | false -> ":)"
            | true -> "X|"
            printfn "Computer: %i | %s" c.Id state)
        printfn "================"

    // Underlying connection.
    member this.Connection = connection

    // Makes move during which computers can be contaminated.
    member this.MakeMove() =
        connection.GetAll() |> makeMove