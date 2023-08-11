let rec plast_two = function
  | [ x; y ] -> x, y
  | [] -> raise Not_found
  | _ :: tl -> plast_two tl
;;

let () =
  let ls = [ 1; 2; 3; 4; 5 ] in
  let f, s = plast_two ls in
  Printf.printf "first: %d; second: %d" f s
;;
