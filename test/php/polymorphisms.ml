(* Polymorphism test *)
let id x = x

let test_list_poly () =
  let l1 = [1; 2; 3] in
  let l2 = ["a"; "b"; "c"] in
  print_int (List.length l1); print_string " ";
  print_int (List.length l2); print_newline ()

let test_generic_compare () =
  print_string (if 1 = 1 then "true" else "false"); print_string " ";
  print_string (if "a" = "a" then "true" else "false"); print_string " ";
  print_string (if [1; 2] = [1; 2] then "true" else "false"); print_newline ()

let () = 
  print_string "ID: "; print_int (id 42); print_newline ();
  print_string "List: "; test_list_poly ();
  print_string "Compare: "; test_generic_compare ()
