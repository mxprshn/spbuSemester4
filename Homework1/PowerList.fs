module PowerList

let powerList n m =

    let rec power x n acc =
        match n with
        | 0 -> 1
        | 1 -> x * acc
        | _ when n % 2 = 0 -> power (x * x) (n / 2) acc
        | _ -> power (x * x) (n / 2) (acc * x)      

    let rec powerListRecursive n m i acc =
        if i = m then
            Some(acc)
        else powerListRecursive n m (i + 1) ((acc.Head / 2) :: acc)

    if (m < 0) || (n < 0) then
        None
    else
        powerListRecursive n m 0 [power 2 (n + m) 1]