(*
   14. Duplicate the elements of a list. (easy)
   Solution

   # duplicate ["a"; "b"; "c"; "c"; "d"];;
   - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)

let rec duplicate = function
  | x :: xs -> x :: x :: duplicate xs
  | v -> v
;;

(* Remark: this function is not tail recursive. Can you modify it so it becomes so? *)
(* Sure! *)

let tailrec_duplicate = 
  (* let go acc v = v :: v :: acc  *)
  (* or *)
  let go acc v = acc @ [v; v] (* (With this its even pointfree!) *)
  in List.fold_left go []

let ls = [ "a"; "b"; "c"; "c"; "d" ]
let expected = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
let () = assert (duplicate ls = expected)
let () = assert (tailrec_duplicate ls = expected)
