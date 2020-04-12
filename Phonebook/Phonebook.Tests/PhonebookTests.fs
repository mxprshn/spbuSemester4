module Phonebook.Tests

open NUnit.Framework
open FsUnit
open Phonebook

[<Test>]
let ``Count for empty book returns 0`` () =
    Phonebook () |> Phonebook.Count |> should equal 0

[<Test>]
let ``Added name exists in the book`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.NameExists "Sheldon Cooper" book |> should be True

[<Test>]
let ``Added number exists in the book`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.NumberExists "555-555-555" book |> should be True

[<Test>]
let ``Not added name does not exist in the book`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.NameExists "Peter Gregory" book |> should be False

[<Test>]
let ``Not added number does not exist in the book`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.NumberExists "111-111-111" book |> should be False

[<Test>]
let ``Find number by name`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.FindNumber "Sheldon Cooper" book |> should equal (Some("555-555-555"))

[<Test>]
let ``Find name by number`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.FindName "555-555-555" book |> should equal (Some("Sheldon Cooper"))
    
[<Test>]
let ``Try to find name which does not exist`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.FindName "111-111-111" book |> should equal None

[<Test>]
let ``Try to find number which does not exist`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555"
    Phonebook.FindNumber "Peter Gregory" book |> should equal None

[<Test>]
let ``Try to add number which already exists`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555" |>
        Phonebook.Add "Howard Wolowitz" "555-555-555"
    Phonebook.NameExists "Howard Wolowitz" book |> should be False
    Phonebook.Count book |> should equal 1

[<Test>]
let ``Try to add name which already exists`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555" |>
        Phonebook.Add "Sheldon Cooper" "111-111-111"
    Phonebook.FindNumber "Sheldon Cooper" book |> should equal (Some("555-555-555"))
    Phonebook.Count book |> should equal 1

[<Test>]
let ``Read data from file`` () =
    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555" |> Phonebook.FromFile "Test1.txt"

    Phonebook.FindNumber "Gavin Belson" book |> should equal (Some("3-14-15-926"))
    Phonebook.FindNumber "Sheldon Cooper" book |> should equal (Some("555-555-555"))
    Phonebook.FindNumber "Walter White" book |> should equal (Some("111-111-111"))

    Phonebook.Count book |> should equal 3

[<Test>]
let ``Write and read data from file`` () =
    if System.IO.File.Exists "Test2.txt" then
        System.IO.File.Delete "Test2.txt"

    let book = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555" |>
        Phonebook.Add "Gavin Belson" "3-14-15-926" |>
        Phonebook.Add "Walter White" "111-111-111"

    Phonebook.ToFile "Test2.txt" book

    let bookFromFile = Phonebook () |> Phonebook.FromFile "Test2.txt" 
    Phonebook.FindNumber "Gavin Belson" bookFromFile |> should equal (Some("3-14-15-926"))
    Phonebook.FindNumber "Sheldon Cooper" bookFromFile |> should equal (Some("555-555-555"))
    Phonebook.FindNumber "Walter White" bookFromFile |> should equal (Some("111-111-111"))

    Phonebook.Count bookFromFile |> should equal 3

[<Test>]
let ``List generated from book contains all records`` () =
    let list = Phonebook () |> Phonebook.Add "Sheldon Cooper" "555-555-555" |>
        Phonebook.Add "Gavin Belson" "3-14-15-926" |>
        Phonebook.Add "Walter White" "111-111-111" |> Phonebook.ToList

    list |> should contain ("Sheldon Cooper", "555-555-555")
    list |> should contain ("Gavin Belson", "3-14-15-926")
    list |> should contain ("Walter White", "111-111-111")
    list.Length |> should equal 3