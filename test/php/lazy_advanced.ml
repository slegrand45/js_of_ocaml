(* Advanced Lazy test *)

let test_lazy_basic () =
  let l = lazy (print_string "Evaluating..."; 42) in
  print_string "Before force ";
  print_int (Lazy.force l); print_string " ";
  print_int (Lazy.force l); print_newline ()

let test_lazy_exception () =
  let l = lazy (raise (Failure "lazy error")) in
  try
    ignore (Lazy.force l)
  with Failure s ->
    print_string "Caught lazy error: "; print_string s; print_newline ();
    try
      ignore (Lazy.force l)
    with Failure _ ->
      print_string "Caught again (memoized exception)"; print_newline ()

let () =
  test_lazy_basic ();
  test_lazy_exception ()
