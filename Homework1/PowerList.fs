module PowerList

let rec power x n acc =
    if (n = 0) then
        1
    else if (n = 1) then
        x * acc
    else if (n % 2 = 0) then
        power (x * x) (n / 2) acc
    else
        power (x * x) (n / 2) (acc * x)

let rec powerList n m i acc =
    if (m < 0) || (n < 0) then
        None
    else if i = m + 1 then
        Some(acc)
    else
        powerList n m (i + 1) ((power 2 (n + m - i) 1) :: acc)