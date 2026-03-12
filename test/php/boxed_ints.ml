(* Boxed integers test *)
let test_boxed () =
  let x = 42 in
  let y = x + 1 in
  y
let () = 
  print_int (test_boxed ());
  print_newline ()
