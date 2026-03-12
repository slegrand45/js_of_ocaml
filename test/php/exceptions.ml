(* Exceptions test *)
exception MyExn
let test_exn () =
  try raise MyExn with MyExn -> 42
let () = print_int (test_exn ()); print_newline ()
