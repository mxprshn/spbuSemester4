module Phonebook

/// Class implementing data structure to store names and telephone numbers.
type Phonebook(records) =
    let records = Map<string, string> records
    member this.Records with private get () = records

    /// Constructor.
    new () = Phonebook []

    /// Finds number in the book by the specified name.
    static member FindNumber name (book: Phonebook) = Map.tryFind name book.Records

    /// Finds name in the book by the specified number.
    static member FindName number (book: Phonebook) = Map.tryFindKey (fun na nu -> nu = number) book.Records

    /// Adds name and number to the book if they don't already exist.
    static member Add name number (book: Phonebook) =
        if not <| Phonebook.NameExists name book && not <| Phonebook.NumberExists number book then
            book.Records |> Map.add name number |> Map.toList |> Phonebook
        else book

    /// Converts the book to the list of pairs.
    static member ToList (book: Phonebook) = book.Records |> Map.toList

    /// Reads records from the file and adds them to the book.
    static member FromFile path book = System.IO.File.ReadLines path |> Seq.chunkBySize 2 |>
        Seq.fold (fun b l -> if l.Length = 2 then Phonebook.Add l.[0] l.[1] b else b) book

    /// Creates file if it does not exist and writes data from the book to it.
    static member ToFile path (book: Phonebook) = System.IO.File.WriteAllLines (path, book.Records |> Map.toList |>
        List.map (fun (na, nu) -> [na; nu]) |> List.concat)
    
    /// Checks if the specified name exists in the book.
    static member NameExists name (book: Phonebook) = Map.containsKey name book.Records

    /// Checks if the specified number exists in the book.
    static member NumberExists number (book: Phonebook) = Map.exists (fun na nu -> nu = number) book.Records

    /// Returns the number of records in the book.
    static member Count (book: Phonebook) = Map.count book.Records