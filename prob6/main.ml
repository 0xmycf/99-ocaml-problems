(*
   6. Find out whether a list is a palindrome. (easy)

   HINT: a palindrome is its own reverse.
*)

let is_palindrome ls = List.fold_left (fun acc v -> v :: acc) [] ls = ls
let lso = [ 1; 2; 3; 2; 1 ]
let lse = [ 1; 2; 2; 1 ]
let lsn = [ 2; 2; 1 ]

let () =
  assert (is_palindrome lso);
  assert (is_palindrome lse);
  assert (not @@ is_palindrome lsn)
;;
