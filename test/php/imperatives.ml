(* Imperative code *)
let test_imp () =
  let r = ref 0 in
  r := !r + 1;
  !r
let () = print_int (test_imp ()); print_newline ()
