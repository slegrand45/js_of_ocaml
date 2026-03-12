(* Labels test *)
let test_labels () =
  let f ~x ~y = x + y in
  f ~x:1 ~y:2
let () = print_int (test_labels ()); print_newline ()
