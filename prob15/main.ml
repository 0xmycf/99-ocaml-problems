(*
   15. Replicate the elements of a list a given number of times. (medium)
*)

let rec replicate ls n =
  match ls with
  | [] -> []
  | hd :: tl -> List.init n (Fun.const hd) @ replicate tl n
;;

let input = [ "a"; "b"; "c" ], 3
let expected = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
let uncurry f (a, b) = f a b
let () = assert ((uncurry @@ replicate) input = expected)

let () =
  let ans = (uncurry @@ replicate) input in
  print_newline ();
  List.iter (Printf.printf "%s\n") ans
;;
