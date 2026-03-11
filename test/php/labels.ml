(* Labels test *)
let test_labels () =
  let f ~x ~y = x + y in
  f ~x:1 ~y:2
let () = ignore (test_labels ())
