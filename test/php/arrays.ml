(* Arrays test *)
let test_basic () =
  let a = [| 1; 2; 3 |] in
  let x = a.(0) in
  a.(1) <- 10;
  x + a.(1)

let test_blit () =
  let a1 = [| 1; 2; 3; 4; 5 |] in
  let a2 = Array.make 5 0 in
  Array.blit a1 1 a2 2 3;
  (* a2 should be [| 0; 0; 2; 3; 4 |] *)
  a2

let test_fill () =
  let a = Array.make 5 0 in
  Array.fill a 1 3 42;
  (* a should be [| 0; 42; 42; 42; 0 |] *)
  a

let test_init () =
  let a = Array.init 5 (fun i -> i * i) in
  a

let test_matrix () =
  let m = Array.make_matrix 3 3 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      m.(i).(j) <- i + j
    done
  done;
  m

let print_array a =
  print_char '[';
  for i = 0 to Array.length a - 1 do
    print_int a.(i);
    if i < Array.length a - 1 then print_string "; "
  done;
  print_char ']'

let () = 
  print_string "Basic: "; print_int (test_basic ()); print_newline ();
  print_string "Blit: "; print_array (test_blit ()); print_newline ();
  print_string "Fill: "; print_array (test_fill ()); print_newline ();
  print_string "Init: "; print_array (test_init ()); print_newline ();
  print_string "Matrix: ";
  let m = test_matrix () in
  for i = 0 to 2 do
    print_array m.(i);
    if i < 2 then print_string ", "
  done;
  print_newline ()
