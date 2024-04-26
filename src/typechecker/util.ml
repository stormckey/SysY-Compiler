open Lib.Err

let sp = Printf.sprintf
let pe = print_endline

let zip l1 l2 =
  let rec aux l1 l2 acc =
    match (l1, l2) with
    | [], [] -> List.rev acc
    | h1 :: t1, h2 :: t2 -> aux t1 t2 ((h1, h2) :: acc)
    | _, _ ->
        raise_semantic_error
          "the length of args is different from the length of params"
  in
  aux l1 l2 []

let rec drop_head_n l n =
  if n <= 0 then l
  else match l with [] -> [] | _ :: tl -> drop_head_n tl (n - 1)
