(* Mutable variables test *)
let test_mutvar () =
  let x = ref 0 in
  x := 1;
  !x
let () = ignore (test_mutvar ())
