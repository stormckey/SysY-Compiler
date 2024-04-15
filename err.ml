open Type

let id_not_found_error id = failwith (Printf.sprintf "id:%s is not defined" id)

let raise_type_mismatch_error ty1 ty2 =
  failwith
    (Printf.sprintf "Type mismatch, expect %s but get %s"
       (string_of_value_type ty1) (string_of_value_type ty2))

let pt tree =
  PrintBox_text.output stdout tree;
  print_newline ()

let check_type_mismatch tree2 ty1 ty2 tree1 =
  if ty1 <> ty2 then (
    pt tree1;
    print_endline "----------------in------------------";
    pt tree2;
    raise_type_mismatch_error ty1 ty2)

let fail_with_tree tree info =
  pt tree;
  failwith info
