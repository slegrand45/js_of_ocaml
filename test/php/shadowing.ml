(* Shadowing test *)
let test_shadow () =
  let x = 1 in
  let x = x + 1 in
  x
let () = ignore (test_shadow ())
