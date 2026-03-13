(* Sys and Filename test *)

let test_sys () =
  print_string "OS: ";
  let os = Sys.os_type in
  (* Normalize OS for test comparison if necessary, 
     but let's see what it returns by default *)
  print_string os;
  print_newline ();
  print_string "Word size: "; print_int Sys.word_size; print_newline ()

let test_filename () =
  let path = "/path/to/file.txt" in
  print_string "Dirname: "; print_string (Filename.dirname path); print_newline ();
  print_string "Basename: "; print_string (Filename.basename path); print_newline ();
  print_string "Extension: "; print_string (Filename.extension path); print_newline ()

let () =
  test_sys ();
  test_filename ()
