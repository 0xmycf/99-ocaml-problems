(*
4. Find the number of elements of a list. (easy)

OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.
*)

let len = 
    let rec go acc = function
    | [] -> acc
    | _ :: tl -> go (succ acc) tl
    in go 0

(*
  Other solutions:
*)

(*Not tail-recursive*)
let _len_foldr ls = List.fold_right (fun _ acc -> succ acc) ls 0

(* I think this is also tail-recursive but I am not sure *)
let _len_foldl ls = List.fold_left (fun acc _ -> succ acc) 0 ls


let () = 
    let ls = List.init 10 (fun _ -> 0) in
    assert (len ls = 10)
