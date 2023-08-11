let rec plast = function
  | [ x ] -> x
  | [] -> raise Not_found
  | _ :: tl -> plast tl
;;

let () =
  let ls = [ 1; 2; 3; 4; 5 ] in
  let last_elem = plast ls in
  print_int last_elem
;;
