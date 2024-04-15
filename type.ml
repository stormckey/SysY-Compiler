open Core

exception SemanticError of string
exception SemanticErrorWithCurTree of string * PrintBox.t
exception SemanticErrorWithCurTreeParentTree of string * PrintBox.t * PrintBox.t

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
    raise
      (SemanticError
         (Printf.sprintf "type %s is incompatible with %s."
            (value_type_to_string a) (value_type_to_string b)))

let id_not_found_error id = failwith (Printf.sprintf "id:%s is not defined" id)
