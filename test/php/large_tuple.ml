(* Large tuple/block test *)

let make_large_tuple () =
  (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
   20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
   40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
   60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
   80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99)

let test_large_tuple () =
  let t = make_large_tuple () in
  let (v0, _, _, _, _, _, _, _, _, _, v10, _, _, _, _, _, _, _, _, _,
       v20, _, _, _, _, _, _, _, _, _, v30, _, _, _, _, _, _, _, _, _,
       v40, _, _, _, _, _, _, _, _, _, v50, _, _, _, _, _, _, _, _, _,
       v60, _, _, _, _, _, _, _, _, _, v70, _, _, _, _, _, _, _, _, _,
       v80, _, _, _, _, _, _, _, _, _, v90, _, _, _, _, _, _, _, _, v99) = t in
  print_int v0; print_newline ();
  print_int v10; print_newline ();
  print_int v20; print_newline ();
  print_int v30; print_newline ();
  print_int v40; print_newline ();
  print_int v50; print_newline ();
  print_int v60; print_newline ();
  print_int v70; print_newline ();
  print_int v80; print_newline ();
  print_int v90; print_newline ();
  print_int v99; print_newline ()

let () = test_large_tuple ()
