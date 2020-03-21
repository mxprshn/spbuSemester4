module Interpreter

type Expression = 
    | Var of char
    | App of Expression * Expression
    | Abs of char * Expression

let rec isFree e v =
    match e with
    | Var(x) -> x = v
    | App(l, r) -> isFree l v || isFree r v
    | Abs(x, e) -> x <> v && isFree e v

let rec isBound e v =
    match e with
    | Abs(x, e) -> x = v || isBound e v
    | Var(x) -> false
    | App(l, r) -> isBound l v || isBound r v

let rec subs b a = function
    | Abs(v, e) when not (isFree a v) -> Abs(v, subs b a e)
    | Var(v) when v = b -> Var(a)
    | Var(v) as x -> x
    | App(l, r) -> App(subs b a l, subs b a r)
    | Abs(v, e) as x when v = b -> x
    | Abs(v, e) ->
        let newSym = ['a'..'z'] |> List.filter (not (isFree a)) |> List.head
        Abs(newSym, subs(b, a, subs(b, Var(newSym), e))

let rec reduce expression =
    match expression with
    | Var(v) as x -> x
    | App(Abs(v, e), r) -> reduce(subs v r e) 
    | Abs(v, e) -> Abs(v, reduce e)