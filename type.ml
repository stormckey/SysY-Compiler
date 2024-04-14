open Core

type value_type =
  | IntType
  | VoidType
  | ArrayType of int list
  | FuncType of value_type * value_type list
[@@deriving equal]

let rec string_of_value_type value_type =
  match value_type with
  | IntType -> "int"
  | VoidType -> "void"
  | ArrayType [] -> "int*"
  | ArrayType int_list ->
      "int(*)"
      ^ String.concat (List.map int_list ~f:(fun i -> Printf.sprintf "[%d]" i))
  | FuncType (return_type, paras) ->
      string_of_value_type return_type
      ^ " ("
      ^ String.concat (List.map paras ~f:string_of_value_type)
      ^ ")"

let ( == ) a b = equal_value_type a b
let ( != ) a b = not (a == b)
