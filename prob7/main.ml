(*
7. Flatten a nested list structure. (medium)

# (* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
*)

type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten = function
    | [] -> []
    | One a :: tl -> a :: flatten tl
    | Many aa :: tl -> flatten aa @ flatten tl

let ls = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
let expect = ["a"; "b"; "c"; "d"; "e"];;

let () = List.iter begin Printf.printf " %s " end begin flatten ls end;;
let () = assert (flatten ls = expect);;
