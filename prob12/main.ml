(*
  12. Decode a run-length encoded list. (medium)
  Given a run-length code list generated as specified in the previous problem, construct its uncompressed version

  # decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
  - : string list =
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)

 type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode ls = let open List in 
    let go = function
    | One a -> [a]
    | Many (cnt, a) -> init cnt (Fun.const a)
    in flatten @@ map go ls  (* There should be a List.flat_map in the List module *)

let ls = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
let expected = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
let () = assert (decode ls = expected)
