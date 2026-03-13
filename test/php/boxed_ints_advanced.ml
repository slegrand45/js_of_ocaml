(* 32-bit, 64-bit and Native integers test *)

let test_int32 () =
  let x = Int32.of_int 100 in
  let y = Int32.add x (Int32.of_int 200) in
  print_string (Int32.to_string y); print_newline ()

let test_int64 () =
  let x = Int64.of_int 1000 in
  let y = Int64.mul x (Int64.of_int 2000) in
  print_string (Int64.to_string y); print_newline ()

let test_nativeint () =
  let x = Nativeint.of_int 50 in
  let y = Nativeint.sub x (Nativeint.of_int 10) in
  print_string (Nativeint.to_string y); print_newline ()

let () =
  print_string "Int32: "; test_int32 ();
  print_string "Int64: "; test_int64 ();
  print_string "Nativeint: "; test_nativeint ()
