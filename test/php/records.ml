(* Records test *)
type t = { x : int; mutable y : int }

let test_records () =
  let r = { x = 10; y = 20 } in
  print_int r.x;
  print_newline ();
  print_int r.y;
  print_newline ();
  
  (* Mutable update *)
  r.y <- 30;
  print_int r.y;
  print_newline ();
  
  (* Functional update *)
  let r2 = { r with x = 40 } in
  print_int r2.x;
  print_newline ();
  print_int r2.y;
  print_newline ()

let () = test_records ()
