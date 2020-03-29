module BalanceChecker

/// Checks bracket balance in the given string.
let checkBalance str =
    let left = ['('; '['; '{'] |> Set.ofList
    let right = [')'; ']'; '}'] |> Set.ofList
    let pairs = [')', '('; ']', '['; '}', '{'] |> Map.ofList
    let rec checkBalanceRecursive brackets str =
        match str with
        | h::t when Set.contains h left -> checkBalanceRecursive (h::brackets) t
        | h::t when Set.contains h right -> 
            match brackets with
            | bh::bt when bh = Map.find h pairs -> checkBalanceRecursive bt t
            | _ -> false
        | [] -> List.isEmpty brackets
        | _::t -> checkBalanceRecursive brackets t
    str |> Seq.toList |> checkBalanceRecursive []