(* Printf and Format test *)

let test_printf () =
  Printf.printf "Int: %d, String: %s, Float: %.2f\n" 42 "hello" 3.14159

let test_sprintf () =
  let s = Printf.sprintf "Hex: %x, Bool: %b" 255 true in
  print_endline s

let test_format () =
  Format.printf "Format: @[%d@ %s@]\n" 100 "elements"

let () =
  test_printf ();
  test_sprintf ();
  test_format ()
