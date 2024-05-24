(*
   23. Extract a given number of randomly selected elements from a list. (medium)

   The selected items shall be returned in a list. We use the Random module but do not initialize it with Random.self_init for reproducibility.
*)

(** @param n = amount of values to pick from the list *)
let rand_select xs n =
  let aux xs =
    let idx = Random.int (List.length xs - 1) in
    List.nth xs idx
  in
  List.init n (fun _ -> aux xs)
;;

let test xs ys amount =
    List.length xs == amount
  &&
    List.fold_left
    (fun acc x -> acc && List.mem x ys)
    true
    xs
  [@@ocamlformat "disable"]

let () =
  let total = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] in
  let res = rand_select total 3 in
  let _list_example = [ "g"; "d"; "a" ] in
  assert (test res total 3);
  for i = 1 to List.length total - 1 do
    let res = rand_select total i in
    List.iter (Printf.printf "%s ") res;
    print_newline ();
    assert (test res total i)
  done
;;
