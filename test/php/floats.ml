(* Floats test *)
let test_floats () =
  let x = 1.5 in
  let y = 2.5 in
  let _ = x +. y in
  let _ = x -. y in
  let _ = x *. y in
  let _ = x /. y in
  x
let () = print_float (test_floats ()); print_newline ()
