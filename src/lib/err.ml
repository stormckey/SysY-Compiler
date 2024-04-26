open Core

exception SemanticError of string
exception SemanticErrorWithCurTree of string * PrintBox.t
exception SemanticErrorWithCurTreeParentTree of string * PrintBox.t * PrintBox.t

let raise_semantic_error msg = raise (SemanticError msg)

let catch_raise_with_ast f tr =
  try f () with
  | SemanticErrorWithCurTree (msg, tree) ->
      raise (SemanticErrorWithCurTreeParentTree (msg, tree, tr))
  | SemanticError msg -> raise (SemanticErrorWithCurTree (msg, tr))

let printSemanticError msg tree1 tree2 =
  print_endline ("Semantic Error: " ^ msg);
  Option.iter tree1 ~f:(fun tree1 -> PrintBox_text.output stdout tree1);
  Option.iter tree2 ~f:(fun tree2 ->
      print_endline "\n-----------------in--------------------\n";
      PrintBox_text.output stdout tree2);
  exit 1

let handleSemanticError f =
  try f () with
  | SemanticErrorWithCurTreeParentTree (msg, tree1, tree2) ->
      printSemanticError msg (Some tree1) (Some tree2)
  | SemanticErrorWithCurTree (msg, tree) ->
      printSemanticError msg (Some tree) None
  | SemanticError msg -> printSemanticError msg None None
