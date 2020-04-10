module Square

/// Prints square of with the specified size length.
let printSquare n =
    let rec printSquareRecursive n x y =
        match (x, y) with
        | _ when y = n -> printf ""
        | _ when x = n ->
            printf "\n"
            printSquareRecursive n 0 (y + 1)
        | _ when y = 0 || y = n - 1 || x = 0 || x = n - 1 ->
            printf "*"
            printSquareRecursive n (x + 1) y
        | _ -> 
            printf " "
            printSquareRecursive n (x + 1) y
    if n <= 0 then
        printf("Square size should be positive.")
    else
        printSquareRecursive n 0 0

