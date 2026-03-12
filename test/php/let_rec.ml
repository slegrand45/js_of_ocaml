(* Recursive factorial function *)
let rec factorial n = 
  if n <= 1 then 1 
  else n * factorial (n - 1)
let () = print_int (factorial 5); print_newline ()
