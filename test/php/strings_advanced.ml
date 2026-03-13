(* Strings and Bytes advanced test *)

let test_string_ops () =
  let s = "abcdef" in
  print_int (String.length s); print_string " ";
  print_char (String.get s 1); print_string " ";
  print_string (String.sub s 1 3); print_string " ";
  print_string (String.concat "-" ["hello"; "world"]); print_string " ";
  print_string (String.make 3 'x'); print_newline ()

let test_bytes_ops () =
  let b = Bytes.of_string "hello" in
  Bytes.set b 0 'H';
  print_string (Bytes.to_string b); print_string " ";
  let b2 = Bytes.copy b in
  Bytes.set b2 4 'O';
  print_string (Bytes.to_string b); print_string " ";
  print_string (Bytes.to_string b2); print_newline ()

let test_escapes () =
  let s = "line1\nline2\t\"quoted\"" in
  print_int (String.length s); print_newline ()

let () =
  print_string "String ops: "; test_string_ops ();
  print_string "Bytes ops: "; test_bytes_ops ();
  print_string "Escapes: "; test_escapes ()
