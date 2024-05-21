(*
   20. Remove the K'th element from a list. (easy)

   The first element of the list is numbered 0, the second 1,...
*)

let remove_at n ls =
  let aux acc v =
    let m, ls = acc in
    if m = 0 then
      m - 1, ls
    else
      m - 1, v :: ls
  in
  let _, ret = List.fold_left aux (n, []) ls in
  List.rev ret
;;

let () =
  let res = remove_at 1 [ "a"; "b"; "c"; "d" ] in
  let list = [ "a"; "c"; "d" ] in
  assert (list = res)
;;
