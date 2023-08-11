(*
   8. Eliminate consecutive duplicates of list elements. (medium)
*)

let ls = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
let expect = [ "a"; "b"; "c"; "a"; "d"; "e" ]

let rec compress = function
  | a :: b :: tl ->
    (* you can name full cases with 'as <ident>' syntax *)
    if a = b then
      compress (b :: tl)
    else
      a :: compress (b :: tl)
  | a -> a
;;

let () = List.iter (Printf.printf " %s ") (compress ls)
let () = assert (compress ls = expect)
