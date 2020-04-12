open System
open Phonebook

[<EntryPoint>]
let main argv =
    let rec phoneBookLoop book =
        match Console.ReadLine () with
        | "0" -> 0
        | "1" ->
            printfn "%A" (Phonebook.ToList book)
            phoneBookLoop book
        | "2" ->
            printf "Enter name: "
            let name = Console.ReadLine ()
            printf "Enter number: "
            let number = Console.ReadLine ()

            if Phonebook.NameExists book name then
                printfn "The name already exists in the book."
                phoneBookLoop book
            elif Phonebook.NumberExists book number then
                printfn "The number already exists in the book."
                phoneBookLoop book
            else phoneBookLoop <| Phonebook.Add book name number
        | "3" -> 
            printf "Enter name to find the number: "
            let name = Console.ReadLine ()
            match Phonebook.FindNumber book name with
            | None ->
                printfn "The name does not exist."
                phoneBookLoop book
            | Some(number) ->
                printfn "%s : %s" name number
                phoneBookLoop book
        | "4" ->
            printf "Enter number to find the name: "
            let number = Console.ReadLine ()
            match Phonebook.FindName book number with
            | None ->
                printfn "The number does not exist."
                phoneBookLoop book
            | Some(name) ->
                printfn "%s : %s" name number
                phoneBookLoop book
        | "5" ->
            printf "Enter path of the file to read data from: "
            let path = Console.ReadLine ()
            phoneBookLoop <| Phonebook.FromFile book path
        | "6" ->
            printf "Enter path of the file to write data: "
            let path = Console.ReadLine ()
            Phonebook.ToFile book path
            phoneBookLoop book
        | _ -> phoneBookLoop book

    phoneBookLoop <| Phonebook ()
