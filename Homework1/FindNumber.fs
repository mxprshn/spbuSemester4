module FindNumber

let rec findNumber list n i =
    if list = [] then
        None
    else if list.Head = n then
        Some(i)
    else
        findNumber list.Tail n (i + 1)