(*
  11. Modified run-length encoding. (easy)
  Modify the result of the previous problem in such a way
  that if an element has no duplicates it is simply copied 
  into the result list. Only elements with duplicates are transferred as (N E) lists.

  Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

  # type 'a rle =
      | One of 'a
      | Many of int * 'a;;
  type 'a rle = One of 'a | Many of int * 'a


  # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  - : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")]

*)

(* You can re-use the previous solution *)
let prob9 = 
    let go v acc = 
      match acc with
      | (ihd :: _ as iacc) :: tl 
        when ihd = v -> (v :: iacc) :: tl
      | _ -> [v] :: acc
  in fun e -> List.fold_right go e []

 type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode ls = let open List in
    let go subls = 
      match length subls with
      | 1 -> One (hd subls)
      | x -> Many (x, hd subls)
    in List.map go (prob9 ls)

let ls = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
let expected = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
let () = assert (encode ls = expected)
