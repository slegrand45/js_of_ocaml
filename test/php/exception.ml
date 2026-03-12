(* Exception raising *)
exception MyException

let raise_exn () = 
  raise MyException

let () =
  try raise_exn () with MyException -> print_string "Raised MyException"; print_newline ()
