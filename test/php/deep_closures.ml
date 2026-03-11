(* Deep nested closures test *)

let test_deep_closures () =
  let a = 1 in
  let f1 b =
    let c = 2 in
    let f2 d =
      let e = 3 in
      let f3 f =
        let g = 4 in
        let f4 h =
          let i = 5 in
          let f5 j =
            a + b + c + d + e + f + g + h + i + j
          in f5
        in f4
      in f3
    in f2
  in
  let res = f1 10 20 30 40 50 in
  print_int res;
  print_newline ()

let () = test_deep_closures ()
