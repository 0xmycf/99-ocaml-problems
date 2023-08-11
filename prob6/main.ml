(*
6. Find out whether a list is a palindrome. (easy)

HINT: a palindrome is its own reverse.
*)

let is_palindrome ls = begin List.fold_left (fun acc v -> v :: acc) [] ls end = ls
let lso = [1;2;3;2;1]
let lse = [1;2;2;1]
let lsn = [2;2;1]

let () =
    assert begin is_palindrome lso end;
  assert begin is_palindrome lse end;
    assert begin not @@ is_palindrome lsn end
;;
