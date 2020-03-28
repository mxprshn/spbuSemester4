module Interpreter

/// Discriminated union representing a lambda expression.
type Expression = 
    | Var of char
    | App of Expression * Expression
    | Abs of char * Expression

/// Prints a lambda expression in convenient form.
let rec printExp = function
| Var x -> printf "%c" x
| App(l, r) ->
    printf "("
    printExp l
    printf ")("
    printExp r
    printf ")"
| Abs(v, e) ->
    printf "λ%c." v
    printExp e

/// Performs beta reduction using normal strategy.
let rec reduce exp =   

    /// Checks if a variable is free in the expression.
    let rec isFree exp var =
        match exp with
        | Var x -> x = var
        | App(l, r) -> isFree l var || isFree r var
        | Abs(x, e) -> x <> var && isFree e var
    
    /// Performs substitution of a variable in the expression. 
    let rec subs bef aft exp =
        match exp with
        | Abs(v, _) when v = bef -> exp
        | Abs(v, e) when not <| isFree aft v -> Abs(v, subs bef aft e)
        | Abs(v, e) ->
            let newSym = ['a'..'z'] |> List.filter (not << isFree aft) |> List.head
            Abs(newSym, subs bef aft <| subs v (Var newSym) e)
        | Var x when x = bef -> aft
        | Var _ -> exp
        | App(l, r) -> App(subs bef aft l, subs bef aft r)

    /// Reduces all outermost redexes.
    let rec reduceOuter exp =
        match exp with
        | App(l, r) ->
            match reduceOuter l with
            | Abs(v, e) -> subs v r e
            | x -> App(x, r)
        | _ -> exp

    match exp with
    | Var _ -> exp
    | App(l, r) ->
        match reduceOuter l with
        | Abs(v, e) -> reduce <| subs v r e
        | x -> App(reduce x, reduce r)
    | Abs(v, e) -> Abs(v, reduce e)