module Calculate

/// Workflow builder for operations with string integer numbers.
type CalculateBuilder() =
    member this.Bind (x : string, f) =
        match System.Int32.TryParse x with
        | (true, number) -> number |> f
        | _ -> "NaN"
    member this.Return (x : int) =
        x.ToString()