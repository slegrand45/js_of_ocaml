(* test_struct.ml *)
type color = Red | Blue;;

let c = Red in
match c with
| Red -> print_string "Red"; print_newline ()
| Blue -> print_string "Blue"; print_newline ()
