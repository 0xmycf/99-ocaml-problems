(*
   16. Drop every N'th element from a list. (medium)
*)

let drop ls n =
  let rec aux ils i =
    match ils with
    | [] -> []
    | _x :: _xs when i mod n = 0 -> aux _xs (succ i)
    | _x :: _xs -> _x :: aux _xs (succ i)
  in
  aux ls 1
;;

let input1 = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
let input2 = 3
let ans = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]

let () =
  let rans = drop input1 input2 in
  print_newline ();
  List.iter (Printf.printf "%s ") rans;
  print_newline ();
  assert (rans = ans)
;;

