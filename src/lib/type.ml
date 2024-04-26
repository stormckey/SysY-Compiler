open Core
open Err

type value_type =
  | IntType
  | VoidType
  | ArrayType of int list
  | FuncType of value_type * value_type list
[@@deriving equal]

let rec value_type_to_string value_type =
  match value_type with
  | IntType -> "int"
  | VoidType -> "void"
  | ArrayType [] -> "int*"
  | ArrayType int_list ->
      "int(*)"
      ^ String.concat (List.map int_list ~f:(fun i -> Printf.sprintf "[%d]" i))
  | FuncType (return_type, paras) ->
      value_type_to_string return_type
      ^ " ("
      ^ String.concat (List.map paras ~f:value_type_to_string)
      ^ ")"

let ( == ) a b =
  if equal_value_type a b then ()
  else
    raise_semantic_error
      (Printf.sprintf "type %s is incompatible with %s."
         (value_type_to_string a) (value_type_to_string b))
