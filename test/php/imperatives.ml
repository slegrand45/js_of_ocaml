(* Imperative code *)
let test_imp () =
  let r = ref 0 in
  r := !r + 1;
  !r
let () = ignore (test_imp ())
