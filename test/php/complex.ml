(* Complex example with list processing and exceptions *)
exception ProcessError

let process_list lst = 
  try
    match lst with
    | [] -> 0
    | x :: xs -> x + List.length xs
  with _ -> raise ProcessError

let helper x y = x + y
