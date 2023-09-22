(*
   17. Split a list into two parts; the length of the first part is given. (easy)
   If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.

   # split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
   - : string list * string list =
     (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
     # split ["a"; "b"; "c"; "d"] 5;;
   - : string list * string list = (["a"; "b"; "c"; "d"], [])
*)

(* Solution 2: Everything by myself
   (sol 1 was deleted, apparently there is not built in take function?) *)
let split ls len =
  let rec take n ls =
    if n == 0 then
      []
    else (
      match ls with
      | [] -> []
      | hd :: tl -> hd :: take (n - 1) tl)
  in
  let rec drop n ls =
    if n = 0 then
      ls
    else (
      match ls with
      | [] -> []
      | _ :: tl -> drop (n - 1) tl)
  in
  let f = take len ls in
  f, drop len ls
;;

let ls = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
let len = 3
let ans = [ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]

let () =
  let a, b = split ls len in
  print_newline ();
  List.iter (Printf.printf "%s ") a;
  print_newline ();
  List.iter (Printf.printf "%s ") b;
  assert ((a, b) = ans)
;;
