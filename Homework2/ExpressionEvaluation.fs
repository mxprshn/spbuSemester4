module ExpressionEvaluation

type Expression =
    | Sum of Expression * Expression
    | Multiple of Expression * Expression
    | Difference of Expression * Expression
    | Quotient of Expression * Expression
    | Operand of int

let rec evaluate = function
    | Sum(a, b) -> evaluate a + evaluate b
    | Multiple(a, b) -> evaluate a * evaluate b
    | Difference(a, b) -> evaluate a - evaluate b
    | Quotient(a, b) -> evaluate a / evaluate b
    | Operand(x) -> x

printf "%d" (Sum(Operand(1), Operand(2)))