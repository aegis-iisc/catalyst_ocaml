type 'a zipper = Zip of 'a list * 'a list

let empty = Zip ([], [])
let from_list l = Zip ([], l)
let to_list (Zip (ls, rs)) = List.rev ls @ rs

let at_begin = function
            | Zip ([], _) -> true
                | _ -> false
let at_end = function
            | Zip (_, []) -> true
                | _ -> false

let null = function
            | Zip ([], []) -> true
                | _ -> false

let to_begin (Zip (ls, rs)) = Zip ([], List.rev ls @ rs)
let to_end (Zip (ls, rs)) = Zip (List.rev rs @ ls, [])

let cursor (Zip (_, rs)) = match rs with
    | (hd :: _) -> Some hd
        | [] -> None

let go_left = function
            | Zip (a :: ls, rs) -> Zip (ls, a :: rs)
                | z -> z
let go_right = function
            | Zip (ls, a :: rs) -> Zip (a :: ls, rs)
                | z -> z

let insert a (Zip (ls, rs)) = Zip (ls, a :: rs)
let delete = function
            | Zip (ls, _ :: rs) -> Zip (ls, rs)
                | z -> z
let replace a = function
            | Zip (ls, _ :: rs) -> Zip (ls, a :: rs)
                | z -> z
