module TreeMap

type 'T Tree =
    | Node of 'T * 'T Tree * 'T Tree
    | Empty

let rec treeMap f =
    function
    | Empty -> Empty
    | Node(x,l,r) -> Node(f x, treeMap f l,  treeMap f r)