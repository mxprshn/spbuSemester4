module Reverse

let rec reverse list acc =
    if list = [] then
        acc
    else
        reverse list.Tail (list.Head :: acc)