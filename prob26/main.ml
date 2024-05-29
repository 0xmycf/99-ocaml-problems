(*
   26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

   In how many ways can a committee of 3 be chosen from a group of 12 people?
   We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
   For pure mathematicians, this result may be great.
   But we want to really generate all the possibilities in a list.
*)

(* because of dune exec prob26 -w *)
let () = print_newline ()

let take n ls =
  if List.length ls < n then
    None
  else (
    let rec aux n ls =
      match ls with
      | [] -> []
      | hd :: tl ->
        if n = 0 then
          []
        else
          hd :: aux (n - 1) tl
    in
    Some (aux n ls))
;;

(* kind ugly *)
let extract k ls =
  let kmo = k - 1 in
  let rec stuff elem sub =
    match sub with
    | [] -> []
    | _ :: tl as ls ->
      (match take kmo ls with
       | None -> []
       | Some s -> (elem :: s) :: stuff elem tl)
  in
  let rec aux ls =
    match ls with
    | [] -> []
    | hd :: tl -> stuff hd tl @ aux tl
  in
  aux ls
;;

let print_list elem_printer ls =
  print_string "[ ";
  List.iter elem_printer ls;
  print_string "] "
;;

let () =
  let res = extract 2 [ "a"; "b"; "c"; "d" ] in
  let _expected =
    [ [ "a"; "b" ]; [ "a"; "c" ]; [ "a"; "d" ]; [ "b"; "c" ]; [ "b"; "d" ]; [ "c"; "d" ] ]
  in
  (print_list (print_list (Printf.printf "%s; "))) res;
  print_newline ();
  (print_list (print_list (Printf.printf "%s; "))) _expected;
  print_newline ();
  assert (res = _expected)
;;
