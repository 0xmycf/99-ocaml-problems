(* 
  18. Extract a slice from a list. (medium)

  Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).
*)

let slice xs from_ to_ =
  let rec aux xs at acc =
    if at > to_ then acc
    else 
    match xs with
    | hd :: tl when at >= from_ ->  aux tl (succ at) (hd :: acc)
    | _ :: tl -> aux tl (succ at) acc
    | [] -> acc
  in
    List.rev begin aux xs 0 [] end

let () =
  let res = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 in
  let expected = ["c"; "d"; "e"; "f"; "g"] in
  List.iter (Printf.printf "%s") res;
  print_newline ();
  List.iter (Printf.printf "%s") expected;
  print_newline ();
  assert (res = expected)

