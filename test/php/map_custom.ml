(* Map with custom module test *)

module Point = struct
  type t = { x : int; y : int }
  let compare p1 p2 =
    if p1.x <> p2.x then compare p1.x p2.x
    else compare p1.y p2.y
end

module PointMap = Map.Make(Point)

let () =
  let m = PointMap.empty in
  let p1 = { Point.x = 1; y = 2 } in
  let p2 = { Point.x = 2; y = 1 } in
  let m = PointMap.add p1 "p1" m in
  let m = PointMap.add p2 "p2" m in
  print_string (PointMap.find p1 m); print_string " ";
  print_string (PointMap.find p2 m); print_newline ()
