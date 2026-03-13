(* Buffer test *)

let test_buffer () =
  let b = Buffer.create 10 in
  Buffer.add_string b "hello";
  Buffer.add_char b ' ';
  Buffer.add_string b "world";
  print_string (Buffer.contents b); print_newline ();
  print_int (Buffer.length b); print_newline ()

let () =
  print_string "Buffer: "; test_buffer ()
