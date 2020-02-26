module TreeMap

type 'T Tree =
    | Node of 'T * 'T Tree * 'T Tree
    | Empty

let print tree =
    match tree with
    | Node(x,l,r) -> printfn "%d" x
    | Empty -> printfn "%d" -1

let treeFoldBack tree =
    let rec loop t cont = 
        match t with 
        | Empty -> cont(Empty)
        | Node(x,l,r) -> 
            loop l (fun lacc ->
              loop r (fun racc ->
                print lacc
                print racc
                cont(Node(x + 1, lacc, racc))))
    loop tree print

treeFoldBack (Node(0, Node(1, Empty, Node(4, Empty, Empty)), Node(2, Empty, Empty)))


