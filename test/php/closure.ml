(* Closure creation *)
let make_adder x = 
  fun y -> x + y

let () =
  let add5 = make_adder 5 in
  print_int (add5 10);
  print_newline ()
