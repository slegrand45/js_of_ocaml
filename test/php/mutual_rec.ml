(* Mutually recursive functions *)
let rec even n = 
  if n = 0 then true 
  else odd (n - 1)
and odd n = 
  if n = 0 then false 
  else even (n - 1)
let () = 
  print_string (if even 42 then "even" else "odd"); print_newline ();
  print_string (if odd 42 then "odd" else "even"); print_newline ()
