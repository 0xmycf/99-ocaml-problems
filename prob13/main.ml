(*
   Implement the so-called run-length encoding data compression method directly.
   I.e. don't explicitly create the sublists containing the duplicates,
   as in problem "Pack consecutive duplicates of list elements into sublists",
   but only count them. As in problem "Modified run-length encoding",
   simplify the result list by replacing the singleton lists (1 X) by X.

   encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
   - : string rle list =
     [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
*)

type ('a, 'b) t =
  | Many of 'a * 'b
  | One of 'b

let encode ls =
  let open List in
  let go v acc =
    match acc with
    | One c :: tl ->
      if c = v then
        Many (2, v) :: tl
      else
        One v :: acc
    | Many (cnt, c) :: tl ->
      if c = v then
        Many (succ cnt, v) :: tl
      else
        One v :: acc
    | [] -> One v :: acc
  in
  fold_right go ls []
;;

let ls = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

let expected =
  [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;

let () = assert (encode ls = expected)
