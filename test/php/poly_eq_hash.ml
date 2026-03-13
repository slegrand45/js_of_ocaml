(* Polymorphic Equality and Hashing test *)

let test_equality () =
  let x = (1, [2; 3]) in
  let y = (1, [2; 3]) in
  print_string (if x = y then "true" else "false"); print_string " ";
  print_string (if x == y then "true" else "false"); print_newline ()

let test_hashing () =
  let h1 = Hashtbl.hash (1, "a") in
  let h2 = Hashtbl.hash (1, "a") in
  print_string (if h1 = h2 then "true" else "false"); print_newline ()

let () =
  print_string "Eq: "; test_equality ();
  print_string "Hash: "; test_hashing ()
