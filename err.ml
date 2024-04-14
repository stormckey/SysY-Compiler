open Type

let quit_type_mismatch_error ty1 ty2 =
  failwith
    (Printf.sprintf "Type mismatch, expect %s but get %s"
       (string_of_value_type ty1) (string_of_value_type ty2))

let id_not_found_error id = failwith (Printf.sprintf "id:%s is not defined" id)

let check_type_mismatch ty1 ty2 tree1 tree2 =
  if ty1 != ty2 then (
    PrintBox_text.output stdout tree1;
    print_endline "\n -------------------in--------------------";
    PrintBox_text.output stdout tree2;
    print_endline "";
    quit_type_mismatch_error ty1 ty2)
