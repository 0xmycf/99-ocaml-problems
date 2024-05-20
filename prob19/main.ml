(*
   19. Rotate a list N places to the left. (medium)
*)

let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y
  [@@ocamlformat "disable"]

(*
   Trying out some OOP features of Ocaml
*)
class rotator (init : string list) (i : int) =
  object
    val mutable arr : string array = Array.of_list init
    method as_list : string list = Array.to_list arr

    method rotate =
      let new_arr = Array.make (Array.length arr) "" in
      for j = 0 to Array.length arr - 1 do
        let where_to = modulo (j - i) (Array.length arr) in
        new_arr.(where_to) <- arr.(j)
      done;
      arr <- new_arr
  end

let rotate xs i =
  let r = new rotator xs i in
  r#rotate;
  r#as_list
;;

let rec take xs i =
  if i = 0 then
    []
  else (
    match xs with
    | hd :: tl -> hd :: take tl (i - 1)
    | [] -> [])
;;

let rec drop xs i =
  if i = 0 then
    xs
  else (
    match xs with
    | _ :: tl -> drop tl (i - 1)
    | [] -> [])
;;

let takeLast xs =
  let revxs = List.rev xs in
  fun i -> List.rev @@ take revxs i
;;

let rotate_functional xs i =
  let take' =
    if i < 0 then
      takeLast
    else
      take
  in
  let drop' =
    if i < 0 then
      take
    else
      drop
  in
  let first = take' xs (Int.abs i) in
  let last =
    drop'
      xs
      (if i < 0 then
         List.length xs + i
       else
         i)
  in
  if i < 0 then
    first @ last
  else
    last @ first
;;

let () =
  let res = rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 in
  let expected = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ] in
  assert (res = expected)
;;

(* functional *)
let () =
  let res = rotate_functional [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 in
  let expected = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ] in
  assert (res = expected)
;;

let () =
  let res = rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2) in
  let expected = [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ] in
  assert (res = expected)
;;

(* functional *)
let () =
  let res = rotate_functional [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2) in
  let expected = [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ] in
  assert (res = expected)
;;
