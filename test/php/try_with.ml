(* Try-with exception handling *)
let try_exn f = 
  try f () 
  with _ -> 42
let () = print_int (try_exn (fun () -> raise (Failure "test"))); print_newline ()
