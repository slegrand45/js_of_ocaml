(* Simple benchmark test *)
let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2)
let () = 
  print_int (fib 10);
  print_newline ()
