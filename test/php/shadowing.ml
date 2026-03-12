(* Shadowing test *)
let test_shadow () =
  let x = 1 in
  let x = x + 1 in
  x
let () = print_int (test_shadow ()); print_newline ()
