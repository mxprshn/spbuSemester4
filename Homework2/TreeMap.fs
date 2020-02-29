module TreeMap

/// Binary tree representation with discriminated union.
type 'T Tree =
    | Node of 'T * 'T Tree * 'T Tree
    | Empty

/// Returns a new tree got from the source tree by applying a function to all its node values.
let rec treeMap f = function
    | Empty -> Empty
    | Node(x,l,r) -> Node(f x, treeMap f l,  treeMap f r)