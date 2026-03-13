(* Scanf test *)

let test_scanf () =
  let s = "42 hello 3.14" in
  Scanf.sscanf s "%d %s %f" (fun i s f ->
    print_int i; print_string " ";
    print_string s; print_string " ";
    print_float f; print_newline ()
  )

let () =
  print_string "Scanf: "; test_scanf ()
