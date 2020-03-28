module Interpreter

type Expression = 
    | Var of char
    | App of Expression * Expression
    | Abs of char * Expression

let rec printExp = function
    | Var(x) -> printf "%O" x
    | App(l, r) ->
        printf "("
        printExp l
        printf ") "
        printf "("
        printExp r
        printf ")"
    | Abs(x, e) ->
        printf "λ%O." x
        printExp e 

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

let rec subs b a exp =
    match exp with
    | Abs(v, e) as x when v = b -> x
    | Abs(v, e) when not (isFree a v) -> Abs(v, subs b a e)
    | Abs(v, e) ->
        let newSym = ['a'..'z'] |> List.filter (not << (isFree a)) |> List.head
        Abs(newSym, (subs b a (subs v (Var(newSym)) e)))
    | Var(v) when v = b -> a
    | Var(v) as x -> x
    | App(l, r) -> App(subs b a l, subs b a r)

let rec reduce exp = 

    let rec headReduce u =
        printf ":: "
        printExp u
        printf "\n"
        match u with
        | Var(v) as x -> x
        | Abs(a, b) as x -> x
        | App(l, r) ->
            match headReduce l with
                | Abs(v, e) -> subs v r e
                | x -> App(x, r)

    printf "| "
    printExp exp
    printf "\n"
    match exp with
    | Var(v) as x -> x
    | App(l, r) ->
        match headReduce l with
            | Abs(v, e) -> reduce (subs v r e)
            | x -> App(reduce x, reduce r)
    | Abs(v, e) -> Abs(v, reduce e)

let True = Abs('x', Abs('y', Var 'x'))
let False = Abs('x', Abs('y', Var 'y'))
let If = Abs('b', Abs('t', Abs('f', App(App(Var 'b', Var 't'), Var 'f'))))
let And = Abs('a', Abs('b', App(App(App(If, Var 'a'), Var 'b'), False)))
let Or = Abs('a', Abs('b', App(App(App(If, Var 'a'), True), Var 'b')))

reduce (App(App(Or, True), False))
