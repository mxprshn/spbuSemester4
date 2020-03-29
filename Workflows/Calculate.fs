module Calculate

type CalculateBuilder() =
    member this.Bind (x: string, f) =
        match System.Int32.TryParse x with
        | (true, number) -> number |> f
        | _ -> "NaN"
    member this.Return (x: int) =
        x.ToString()

let calculate = CalculateBuilder ()

let ololo = calculate {
    let! a = "4.000"
    return a
}