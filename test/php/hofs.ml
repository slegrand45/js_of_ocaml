(* Higher order functions *)
let apply f x = f x
let double x = x * 2
let test_hof () = apply double 21
let () = print_int (test_hof ()); print_newline ()
