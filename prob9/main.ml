(*
9. Pack consecutive duplicates of list elements into sublists. (medium)

# pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]

*)

(*  This is how you'd do it in haskell 
    I think this is much cleaner than the ocaml version

pack :: Eq a => [a] -> [[a]]
pack = foldr go []
  where
  go v acc = case acc of
      iacc@(ihd:_):tl 
        | ihd == v -> (v:iacc):tl
      _            -> [v]:acc

*)

let pack = 
    let go v acc = 
      match acc with
      | (ihd :: _ as iacc) :: tl 
        when ihd = v -> (v :: iacc) :: tl
      | _ -> [v] :: acc
  in fun e -> List.fold_right go e []


let expected = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
let ls = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]

let () = let open Printf in
  print_newline ();
  List.iter (fun e -> List.iter (printf " %s ") e; print_newline ()) (pack ls);
  print_newline ();
  assert (pack ls = expected)
