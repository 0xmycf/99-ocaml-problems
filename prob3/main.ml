let rec nth ls =
  try
    function
    | 1 -> List.hd ls
    | n -> nth (List.tl ls) (n - 1)
  with
  | _ -> raise @@ Failure "nth"
;;

let rec save_nth ls i =
  try
    match i with
    | 1 -> Some (List.hd ls)
    | n -> save_nth (List.tl ls) (n - 1)
  with
  | _ -> None
;;

let () =
  let n_at =
    save_nth [ "a" ; "b" ] 2
  in
  match n_at with
  | Some x -> Printf.printf "%s\n" x
  | None -> print_endline "None"
;;

let () =
  let n_at =
    save_nth [ "a" ] 2
  in
  match n_at with
  | Some x -> Printf.printf "%s\n" x
  | None -> print_endline "None"
;;

let () =
  let n_at =
    try nth [ "a" ] 2 with
    | _ -> "Not found"
  in
  Printf.printf "%s\n" n_at
;;

let () =
  let n_at =
    try nth [ "a" ; "b" ] 2 with
    | _ -> "Not found"
  in
  Printf.printf "%s\n" n_at
;;

let () =
  let n_at = nth [ "a"; "b"; "c"; "d"; "e" ] 2 in
  print_endline "long list: ";
  Printf.printf "%s\n" n_at
;;
