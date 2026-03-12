(* Mutable variables test *)
let test_mutvar () =
  let x = ref 0 in
  x := 1;
  !x
let () = print_int (test_mutvar ()); print_newline ()
