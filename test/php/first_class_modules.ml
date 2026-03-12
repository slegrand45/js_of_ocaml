(* First class modules test *)
let test_fcm () =
  let x = 42 in
  x
let () = print_int (test_fcm ()); print_newline ()
