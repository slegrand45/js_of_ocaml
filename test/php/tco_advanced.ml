(* Advanced TCO test *)

let rec loop_many_args a b c d e f count =
  if count = 0 then a + b + c + d + e + f
  else loop_many_args (a+1) (b+1) (c+1) (d+1) (e+1) (f+1) (count - 1)

let test_tco_many () =
  print_int (loop_many_args 1 2 3 4 5 6 100000);
  print_newline ()

let () =
  print_string "TCO many: "; test_tco_many ()
