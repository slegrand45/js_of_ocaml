(* UTF-8 string test *)
let s = "Héllô 🌍"

let () =
  print_int (String.length s); (* bytes length *)
  print_newline ();
  print_string s;
  print_newline ()
