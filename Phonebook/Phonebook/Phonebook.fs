module Phonebook

type Phonebook(records) =
    let records = Map<string, string> records
    member this.Records with private get () = records

    new () = Phonebook []

    static member FindNumber (book: Phonebook) name = Map.tryFind name book.Records
    static member FindName (book: Phonebook) number = Map.tryFindKey (fun na nu -> nu = number) book.Records
    static member Add (book: Phonebook) name number = book.Records |> Map.add name number |> Map.toList |> Phonebook
    static member ToList (book: Phonebook) = book.Records |> Map.toList
    static member FromFile book path = System.IO.File.ReadLines path |> Seq.chunkBySize 2 |>
        Seq.fold (fun b l -> if l.Length = 2 then Phonebook.Add b l.[0] l.[1] else b) book
    static member ToFile (book: Phonebook) path = System.IO.File.WriteAllLines (path, book.Records |> Map.toList |>
        List.map (fun (na, nu) -> [na; nu]) |> List.concat)
    static member NameExists (book: Phonebook) name = Map.containsKey name book.Records
    static member NumberExists (book: Phonebook) number = Map.exists (fun na nu -> nu = number) book.Records
