(*
   22. Create a list containing all integers within a given range. (easy)

   If first argument is greater than second, produce a list in decreasing order.
*)

let range from_ to_ =
  let rec aux from_ to_ =
    if from_ = to_ then
      [ to_ ]
    else
      from_ :: aux (succ from_) to_
  in
  let rev =
    if from_ > to_ then
      List.rev
    else
      fun x -> x
  in
  rev @@ aux (Int.min from_ to_) (Int.max from_ to_)
;;

let () =
  let exp1 = range 4 9 in
  let list1 = [ 4; 5; 6; 7; 8; 9 ] in
  let exp2 = range 9 4 in
  let list2 = [ 9; 8; 7; 6; 5; 4 ] in
  assert (exp1 = list1);
  assert (exp2 = list2)
;;
