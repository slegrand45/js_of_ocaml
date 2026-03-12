(* Test mutable references *)
let test_simple_ref () =
  let d = ref 0 in
  d := 1;
  !d

let test_multiple_refs () =
  let r1 = ref 10 in
  let r2 = ref 20 in
  r1 := !r1 + !r2;
  r2 := !r1 * 2;
  !r1 + !r2

let test_complex_refs () =
  let l_ref = ref [1; 2] in
  l_ref := 0 :: !l_ref;
  List.length !l_ref

let () =
  print_int (test_simple_ref ()); print_newline ();
  print_int (test_multiple_refs ()); print_newline ();
  print_int (test_complex_refs ()); print_newline ()
