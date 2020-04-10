module Minimum

open System

/// Finds minimum in the list.
let minimum = function
| [] -> None
| l -> Some(l |> List.fold (fun acc i -> if i < acc then i else acc) Int32.MaxValue)