(*
   10. Run-length encoding of a list. (easy)
   If you need so, refresh your memory about run-length encoding.
   link : https://en.wikipedia.org/wiki/Run-length_encoding

   # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
   - : (int * string) list =
     [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

(* You can re-use the previous solution *)
let prob9 =
  let go v acc =
    match acc with
    | (ihd :: _ as iacc) :: tl when ihd = v -> (v :: iacc) :: tl
    | _ -> [ v ] :: acc
  in
  fun e -> List.fold_right go e []
;;

let encode ls =
  let go v acc =
    match acc with
    | (i, hd) :: tl when hd = v -> (succ i, hd) :: tl
    | _ -> (1, v) :: acc
  in
  List.fold_right go ls []
;;

let ls = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
let expected = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]

let () =
  assert (encode ls = expected);
  assert (List.(map (fun x -> length x, hd x) (prob9 ls) = expected))
;;
