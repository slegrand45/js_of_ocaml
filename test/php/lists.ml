(* Lists test *)

(* Custom head function *)
let hd = function
  | [] -> failwith "empty list"
  | x :: _ -> x

(* Custom tail function *)
let tl = function
  | [] -> failwith "empty list"
  | _ :: xs -> xs

(* Recursive sum function *)
let rec sum l =
  match l with
  | [] -> 0
  | x :: xs -> x + sum xs

let test_lists () =
  (* 1. Create list l = [1; 2; 3; 4; 5] *)
  let l = [1; 2; 3; 4; 5] in
  
  (* 2. Print List.length l *)
  print_int (List.length l);
  print_newline ();
  
  (* 3. Print hd and tl *)
  print_int (hd l);
  print_newline ();
  ignore (tl l);  (* tl is [2; 3; 4; 5], we ignore it *)
  
  (* 4. Define and print sum l *)
  let s = sum l in
  print_int s;
  print_newline ();
  
  (* 5. Use List.map and print length of new list *)
  let doubled = List.map (fun x -> x * 2) l in
  print_int (List.length doubled);
  print_newline ()

let () = test_lists ()
