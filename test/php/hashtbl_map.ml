(* Hashtbl and Map test *)

module IntMap = Map.Make(Int)

let test_map () =
  let m = IntMap.empty in
  let m = IntMap.add 1 "one" m in
  let m = IntMap.add 2 "two" m in
  print_string (IntMap.find 1 m); print_string " ";
  print_string (IntMap.find 2 m); print_string " ";
  print_string (if IntMap.mem 3 m then "true" else "false"); print_newline ()

let test_hashtbl () =
  let h = Hashtbl.create 10 in
  Hashtbl.add h "key1" 100;
  Hashtbl.add h "key2" 200;
  print_int (Hashtbl.find h "key1"); print_string " ";
  print_int (Hashtbl.find h "key2"); print_string " ";
  Hashtbl.replace h "key1" 101;
  print_int (Hashtbl.find h "key1"); print_newline ()

let () =
  print_string "Map: "; test_map ();
  print_string "Hashtbl: "; test_hashtbl ()
