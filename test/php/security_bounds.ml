(* Security bounds test *)
let test_bounds () =
  let a = [| 1; 2; 3 |] in
  if Array.length a > 0 then a.(0) else 0
let () = print_int (test_bounds ()); print_newline ()
