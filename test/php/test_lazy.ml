(* Lazy evaluation test *)
let counter = ref 0
let l = lazy (incr counter; !counter)

let () =
  print_int !counter; (* should be 0 *)
  print_newline ();
  print_int (Lazy.force l); (* should be 1 *)
  print_newline ();
  print_int !counter; (* should be 1 *)
  print_newline ();
  print_int (Lazy.force l); (* should be 1 (memoized) *)
  print_newline ()
