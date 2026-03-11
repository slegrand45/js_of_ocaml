(* Mutations test *)
let test_mut () =
  let r = ref 1 in
  r := !r + 1;
  !r
let () = ignore (test_mut ())
