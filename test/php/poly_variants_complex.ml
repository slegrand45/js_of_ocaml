(* Complex Polymorphic Variants Test *)

let describe_color = function
  | `Red -> "red"
  | `Green -> "green"
  | `Blue -> "blue"
  | `RGB (r, g, b) -> 
      "rgb(" ^ string_of_int r ^ "," ^ string_of_int g ^ "," ^ string_of_int b ^ ")"

let rec sum_variants = function
  | [] -> 0
  | `Val n :: rest -> n + sum_variants rest
  | `Add (a, b) :: rest -> (a + b) + sum_variants rest
  | _ :: rest -> sum_variants rest

let test_complex_pv () =
  (* 1. Basic tags *)
  print_endline (describe_color `Red);
  print_endline (describe_color (`RGB (255, 128, 0)));

  (* 2. Recursive processing of variants with arguments *)
  let data = [ `Val 10; `Add (5, 7); `Val 20 ] in
  let result = sum_variants data in
  print_int result;
  print_newline ();

  (* 3. Polymorphic matching *)
  let f = function
    | `A -> 1
    | `B -> 2
    | `C -> 3
  in
  let g = function
    | `D -> 4
    | (`A | `B | `C) as x -> f x
  in
  print_int (g `D);
  print_int (g `A);
  print_newline ()

let () = test_complex_pv ()
