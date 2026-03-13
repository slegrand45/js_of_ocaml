(* Floats test *)
let test_arith () =
  let x = 1.5 in
  let y = 2.5 in
  print_float (x +. y); print_string " ";
  print_float (x -. y); print_string " ";
  print_float (x *. y); print_string " ";
  print_float (x /. y); print_newline ()

let test_comparison () =
  let x = 1.5 in
  let y = 2.5 in
  print_string (if x < y then "true" else "false"); print_string " ";
  print_string (if x > y then "true" else "false"); print_string " ";
  print_string (if x <= y then "true" else "false"); print_string " ";
  print_string (if x >= y then "true" else "false"); print_string " ";
  print_string (if x = y then "true" else "false"); print_newline ()

let test_special () =
  let pinf = 1.0 /. 0.0 in
  let ninf = -1.0 /. 0.0 in
  let nan = 0.0 /. 0.0 in
  print_float pinf; print_string " ";
  print_float ninf; print_string " ";
  if nan <> nan then print_string "nan is nan" else print_string "nan is NOT nan";
  print_newline ()

let test_functions () =
  print_float (sin 0.0); print_string " ";
  print_float (cos 0.0); print_string " ";
  print_float (sqrt 4.0); print_string " ";
  print_float (exp 0.0); print_string " ";
  print_float (log 1.0); print_newline ()

let () = 
  print_string "Arith: "; test_arith ();
  print_string "Comp: "; test_comparison ();
  print_string "Special: "; test_special ();
  print_string "Func: "; test_functions ()
