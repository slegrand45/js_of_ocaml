(* Char and conversions test *)

let test_char () =
  let c = 'a' in
  print_int (Char.code c); print_string " ";
  print_char (Char.chr 98); print_string " ";
  print_char (Char.uppercase_ascii 'c'); print_newline ()

let test_conversions () =
  print_string (string_of_int 123); print_string " ";
  print_int (int_of_string "456"); print_string " ";
  print_string (string_of_float 1.5); print_string " ";
  print_float (float_of_string "2.5"); print_newline ()

let () =
  print_string "Char: "; test_char ();
  print_string "Conv: "; test_conversions ()
