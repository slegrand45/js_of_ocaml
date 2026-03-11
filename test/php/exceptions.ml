(* Exceptions test *)
exception MyExn
let test_exn () =
  try raise MyExn with MyExn -> 42
let () = ignore (test_exn ())
