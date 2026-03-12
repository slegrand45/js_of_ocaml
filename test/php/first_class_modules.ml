(* First class modules test *)
module type S = sig
  val x : int
  val f : int -> int
end

module M1 = struct
  let x = 10
  let f y = x + y
end

module M2 = struct
  let x = 20
  let f y = x + y
end

let print_m (module M : S) =
  print_int M.x;
  print_char ' ';
  print_int (M.f 5);
  print_newline ()

let () =
  let m1 = (module M1 : S) in
  let m2 = (module M2 : S) in
  print_m m1;
  print_m m2;
  let (module M) = if true then m1 else m2 in
  print_int (M.f 100);
  print_newline ()
