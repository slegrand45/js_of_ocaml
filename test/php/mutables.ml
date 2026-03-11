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
  ignore (test_simple_ref ());
  ignore (test_multiple_refs ());
  ignore (test_complex_refs ())
