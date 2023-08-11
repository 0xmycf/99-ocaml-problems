(*
   5. Reverse a list. (easy)

   OCaml standard library has List.rev but we ask that you reimplement it.
*)

let rev = List.fold_left (fun acc v -> v :: acc) []
let ls = List.init 10 Fun.id

(*
   let () = List.iter begin Printf.printf " %i " end begin rev ls end
   9 8 7 6 5 4 3 2 1 0
*)
let () = assert (rev ls = List.rev ls)
