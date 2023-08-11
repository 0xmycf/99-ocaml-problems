(*
  8. Eliminate consecutive duplicates of list elements. (medium)
*)

let ls = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
let expect = ["a"; "b"; "c"; "a"; "d"; "e"];;
let rec compress = function
  | a :: b :: tl ->  (* you can name full cases with 'as <ident>' syntax *)
      if a = b then
        compress begin b :: tl end 
      else 
        a :: compress begin b :: tl end
  | a -> a

let () = List.iter begin Printf.printf " %s " end begin compress ls end;;
let () = assert (compress ls = expect);;
