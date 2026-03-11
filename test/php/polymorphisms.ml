(* Polymorphism test *)
let id x = x
let test_poly () = id 42
let () = ignore (test_poly ())
