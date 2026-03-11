(* Try-with exception handling *)
let try_exn f = 
  try f () 
  with _ -> 42
