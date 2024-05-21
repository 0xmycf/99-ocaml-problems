(*
   21. Insert an element at a given position into a list. (easy)

   Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)
*)

let rec insert_at elem idx = function
  | hd :: tl when idx = 0 -> elem :: hd :: tl
  | hd :: tl -> hd :: insert_at elem (idx - 1) tl
  | [] -> elem :: []
;;

let () =
  let res1 = insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] in
  let exp1 = [ "a"; "alfa"; "b"; "c"; "d" ] in
  let res2 = insert_at "alfa" 3 [ "a"; "b"; "c"; "d" ] in
  let exp2 = [ "a"; "b"; "c"; "alfa"; "d" ] in
  let res3 = insert_at "alfa" 4 [ "a"; "b"; "c"; "d" ] in
  let exp3 = [ "a"; "b"; "c"; "d"; "alfa" ] in
  assert (res1 = exp1);
  assert (res2 = exp2);
  assert (res3 = exp3)
;;
