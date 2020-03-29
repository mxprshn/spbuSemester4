module Rounding

type RoundingBuilder(prec: int) =
    do
        if prec < 0 || prec > 15 then
            invalidArg "prec" "Must be an integer value between 0 and 15."
    member this.Bind (x: float, f) =
        (x, prec) |> System.Math.Round |> f
    member this.Return (x: float) =
        System.Math.Round (x, prec)



