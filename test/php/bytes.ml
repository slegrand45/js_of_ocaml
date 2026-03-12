(* Bytes test *)
let test_bytes () =
  let b = Bytes.of_string "hello" in
  Bytes.set b 0 'H';
  print_char (Bytes.get b 0);
  print_int (Bytes.length b)

let () = 
  test_bytes ();
  print_newline ()
