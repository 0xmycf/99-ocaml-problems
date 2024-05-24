(*
   25. Generate a random permutation of the elements of a list. (easy)
*)

let permutation ls =
  let len = List.length ls in
  let gen () =
    let idx = Random.int len
    in List.nth ls idx
  in
  List.init
    len
    (fun _ -> gen ())
  [@@ocamlformat "disable"]

let test ls tst len =
  List.length ls = len &&
  List.fold_left
    (fun acc v -> acc && List.mem v tst )
    true
    ls
  [@@ocamlformat "disable"]

let () =
  let ls = [ "a"; "b"; "c"; "d"; "e"; "f" ] in
  let _res = permutation ls in
  let _list = [ "a"; "e"; "f"; "b"; "d"; "c" ] in
  for _ = 0 to 100 do
    let res = permutation ls in
    assert (test res ls (List.length ls))
  done
;;
