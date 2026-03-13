(* Advanced List test *)

let test_list_ops () =
  let l = [1; 2; 3; 4; 5] in
  let doubled = List.map (fun x -> x * 2) l in
  let filtered = List.filter (fun x -> x mod 2 = 0) l in
  let summed = List.fold_left (+) 0 l in
  let reversed = List.rev l in
  
  let print_list l =
    print_char '[';
    let len = List.length l in
    List.iteri (fun i x -> 
      print_int x; 
      if i < len - 1 then print_string "; "
    ) l;
    print_char ']'
  in
  
  print_string "Map: "; print_list doubled; print_newline ();
  print_string "Filter: "; print_list filtered; print_newline ();
  print_string "Fold: "; print_int summed; print_newline ();
  print_string "Rev: "; print_list reversed; print_newline ()

let test_list_assoc () =
  let l = [(1, "one"); (2, "two")] in
  print_string (List.assoc 1 l); print_string " ";
  print_string (if List.mem_assoc 3 l then "true" else "false"); print_newline ()

let () =
  test_list_ops ();
  test_list_assoc ()
