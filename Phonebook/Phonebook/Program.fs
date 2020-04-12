open System
open Phonebook

[<EntryPoint>]
let main argv =
    let rec interfaceLoop book =
        match Console.ReadLine () with
        | "0" -> 0
        | "1" ->
            Phonebook.ToList book |> List.iter (fun (na, nu) -> printfn "%s | %s" na nu)
            interfaceLoop book
        | "2" ->
            printf "Enter name: "
            let name = Console.ReadLine ()
            printf "Enter number: "
            let number = Console.ReadLine ()
            if Phonebook.NameExists name book then
                printfn "The name already exists in the book."
                interfaceLoop book
            elif Phonebook.NumberExists number book then
                printfn "The number already exists in the book."
                interfaceLoop book
            else interfaceLoop <| Phonebook.Add name number book
        | "3" -> 
            printf "Enter name to find the number: "
            let name = Console.ReadLine ()
            match Phonebook.FindNumber name book with
            | None ->
                printfn "The name does not exist."
                interfaceLoop book
            | Some(number) ->
                printfn "%s | %s" name number
                interfaceLoop book
        | "4" ->
            printf "Enter number to find the name: "
            let number = Console.ReadLine ()
            match Phonebook.FindName number book with
            | None ->
                printfn "The number does not exist."
                interfaceLoop book
            | Some(name) ->
                printfn "%s | %s" name number
                interfaceLoop book
        | "5" ->
            printf "Enter path to the file to read data from: "
            let path = Console.ReadLine ()
            if System.IO.File.Exists path then
                interfaceLoop <| Phonebook.FromFile path book
            else
                printfn "File not found or path is invalid."
                interfaceLoop book
        | "6" ->
            printf "Enter path to the file to write data: "
            let path = Console.ReadLine ()
            try
                Phonebook.ToFile path book
            with
            | :? ArgumentException -> printfn "Invalid file path."
            | :? System.IO.DirectoryNotFoundException -> printfn "Directory not found."
            | _ -> printfn "IO error."                
            interfaceLoop book
        | _ -> interfaceLoop book

    printfn "+--------------------------+"
    printfn "| Phonebook commands:      |"
    printfn "| 0 -- exit                |"
    printfn "| 1 -- print current data  |"
    printfn "| 2 -- add new record      |"
    printfn "| 3 -- find number by name |"
    printfn "| 4 -- find name by number |"
    printfn "| 5 -- read data from file |"
    printfn "| 6 -- write data to file  |"
    printfn "+--------------------------+"

    interfaceLoop <| Phonebook ()
