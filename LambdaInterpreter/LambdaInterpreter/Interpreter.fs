module Interpreter

type Expression = 
    | Variable of char
    | Application of Expression * Expression
    | Abstraction of char * Expression

let rec betaReduction expression =
    let rec substitute before after = function
        | Variable(v) when v = before -> Variable(after)
        | Variable(v) as x -> x
        | Application(l, r) -> Application(substitute before after l, substitute before after r) 
        | Abstraction(v, e) -> Abstraction(v, betaReduction e)


    match expression with
        | Variable(v) as x -> x
        | Application(l, r) when l :? Abstraction -> 
        | Abstraction(v, e) -> Abstraction(v, betaReduction e)

