(* Test demonstrating ACC/GRAB environment vs stack semantics issue *)
(* This test should expose when ACC incorrectly accesses stack instead of env *)

(* Simple closure that captures a value and takes an argument *)
let make_adder x =
  fun y -> x + y

(* Test basic closure application *)
let () =
  let add5 = make_adder 5 in
  let result = add5 3 in
  print_int result;  (* Should print 8 *)
  print_newline ()

(* More complex: closure with multiple arguments via GRAB *)
let make_calculator x =
  let multiply y =
    let add z =
      x * y + z
    in
    add
  in
  multiply

let () =
  let calc = make_calculator 3 in
  let mul = calc 4 in
  let result = mul 5 in  (* 3 * 4 + 5 = 17 *)
  print_int result;
  print_newline ()

(* Test closure with recursive call - exercises GRAB + ACC interaction *)
let rec sum n =
  if n <= 0 then 0
  else n + sum (n - 1)

let () =
  let result = sum 5 in  (* 5+4+3+2+1 = 15 *)
  print_int result;
  print_newline ()
