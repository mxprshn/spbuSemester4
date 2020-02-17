module Fibonacci

let rec fibonacci x acc1 acc2 i =
    if x < 0 then
        None
    else if i = x then
        Some(acc1)
    else
        fibonacci x acc2 (acc1 + acc2) (i + 1)