module MultiplyList

// Converting a function to point-free style.
let MultiplyList x l =
    List.map (fun y -> y * x) l

let MultiplyList2 x =
    List.map (fun y -> y * x)

let MultiplyList3 x =
    x |> (*) |> List.map

let MultiplyListPointFree =
    List.map << (*)