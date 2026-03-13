(* test/php/imperatives.ml *)
let test_ref () =
  let r = ref 0 in
  r := !r + 1;
  r := !r * 10;
  !r

let test_for_loop () =
  let sum = ref 0 in
  for i = 1 to 10 do
    sum := !sum + i
  done;
  !sum

let test_for_loop_down () =
  let count = ref 0 in
  for i = 10 downto 1 do
    count := !count + 1
  done;
  !count

let test_while_loop () =
  let i = ref 0 in
  let res = ref 0 in
  while !i < 5 do
    res := !res + !i;
    incr i
  done;
  !res

let test_nested_loops () =
  let count = ref 0 in
  for i = 1 to 3 do
    for j = 1 to 3 do
      count := !count + (i * j)
    done
  done;
  !count

let () =
  print_string "Ref: "; print_int (test_ref ()); print_newline ();
  print_string "For to: "; print_int (test_for_loop ()); print_newline ();
  print_string "For downto: "; print_int (test_for_loop_down ()); print_newline ();
  print_string "While: "; print_int (test_while_loop ()); print_newline ();
  print_string "Nested: "; print_int (test_nested_loops ()); print_newline ()
