module Factorial

let rec factorial x acc i =
    if x < 0 then
        None
    else if i = x + 1 then
        Some(acc)
    else
        factorial x (acc * i) (i + 1)