(* Boxed integers test *)
let test_boxed () =
  let x = 42 in
  let y = x + 1 in
  y
let () = ignore (test_boxed ())
