(* String functions advanced test *)

let test_string_funcs () =
  let s = "  hello world  " in
  print_string "|"; print_string (String.trim s); print_string "|"; print_newline ();
  
  let parts = String.split_on_char ' ' "a b c" in
  List.iter (fun p -> print_string p; print_string "-") parts;
  print_newline ();
  
  let s2 = String.map (fun c -> if c = 'a' then 'x' else c) "banana" in
  print_string s2; print_newline ()

let () =
  test_string_funcs ()
