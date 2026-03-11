type t = { mutable x : int }
let f r = r.x <- 1
let () =
  let r = { x = 0 } in
  f r;
  print_int r.x;
  print_newline ()
