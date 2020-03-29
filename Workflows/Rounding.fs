module Rounding

type RoundingBuilder(prec:int) =
    member this.Bind(x:float, f) =
        let rounded = System.Math.Round(x, prec)
        printfn "%f" rounded
        rounded |> f
    member this.Return(x:float) =
        System.Math.Round(x, prec)

let rounding = RoundingBuilder

let oloo =  rounding 3 {
    let! a = 2.0 / 12.0
    let! b = 3.5
    return a / b
}



