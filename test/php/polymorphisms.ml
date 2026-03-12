(* Polymorphism test *)
let id x = x
let test_poly () = id 42
let () = print_int (test_poly ()); print_newline ()
