module ConcStack

open System

type 'a ConcStack() =
    let mutable elements = []
    let mutable count = 0
    let mutable locker = Object()

    member this.Push (value: 'a) =
        lock locker (fun () ->
        elements <- value::elements
        count <- count + 1)

    member this.TryPop =
        lock locker (fun () ->
        match elements with
        | [] -> None
        | h::t ->
            elements <- t
            count <- count - 1
            Some(h))