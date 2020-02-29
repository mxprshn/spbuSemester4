module ExpressionEvaluation

/// Arithmetical expression tree representation with discriminated union.
type Expression =
    | Sum of Expression * Expression
    | Mult of Expression * Expression
    | Diff of Expression * Expression
    | Quot of Expression * Expression
    | Opnd of int

/// Evaluates an arithmetical expression tree.
let rec evaluate = function
    | Sum(a, b) -> evaluate a + evaluate b
    | Mult(a, b) -> evaluate a * evaluate b
    | Diff(a, b) -> evaluate a - evaluate b
    | Quot(a, b) -> evaluate a / evaluate b
    | Opnd(x) -> x