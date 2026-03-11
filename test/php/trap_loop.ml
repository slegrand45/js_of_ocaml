(* Trap in loop test *)

exception MyExc of int

let rec test_trap n =
  if n = 0 then ()
  else
    try
      if n mod 10 = 0 then raise (MyExc n)
      else test_trap (n - 1)
    with MyExc v ->
      print_string "Caught ";
      print_int v;
      print_newline ();
      test_trap (n - 1)

let () =
  test_trap 100
