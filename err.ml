open Type

let quit_type_mismatch_error ty1 ty2 =
  failwith
    (Printf.sprintf "Type mismatch, expect %s but get %s"
       (string_of_value_type ty1) (string_of_value_type ty2))

let id_not_found_error id = failwith (Printf.sprintf "id:%s is not defined" id)

let check_type_mismatch tree2 ty1 ty2 tree1 =
  if ty1 != ty2 then (
    PrintBox_text.output stdout tree1;
    print_endline "\n -------------------in--------------------";
    PrintBox_text.output stdout tree2;
    print_endline "";
    quit_type_mismatch_error ty1 ty2)

let fail_with_tree tree info =
  PrintBox_text.output stdout tree;
  print_newline ();
  failwith info
