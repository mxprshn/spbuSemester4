module EvenCount

/// Counts the amount of even elements in a list of integer elements.
let evenCountFilter =
    List.filter (fun x -> x % 2 = 0) >> List.length

/// Counts the amount of even elements in a list of integer elements.
let evenCountFold =
    List.fold (fun acc x -> acc + 1 - abs(x % 2)) 0

/// Counts the amount of even elements in a list of integer elements.
let evenCountMap =
    List.map (fun x -> 1 - abs(x % 2)) >> List.fold (+) 0