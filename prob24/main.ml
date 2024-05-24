(*
   24. Lotto: Draw N different random numbers from the set 1..M. (easy)

   The selected numbers shall be returned in a list.
*)

(*
  imperative functional programming????
 *)
let lotto_select select out_of =
  let arr = Array.make select 0 in
  let seen = ref ([] : int list) in
  let upper = out_of + 1 in
  let draw () =
    let rand = ref @@ Random.int upper in
    while List.mem !rand !seen do
      rand := Random.int upper
    done;
    rand
  in
  for i = 0 to select - 1 do
    arr.(i) <- !(draw ())
  done;
  Array.to_list arr
;;

let test thing i j =
  List.length thing = i
    &&
  List.fold_left
    (fun acc x -> acc && x <= j)
    true
    thing
  [@@ocamlformat "disable"]

let () =
  let _res = lotto_select 6 49 in
  let _list = [ 10; 20; 44; 22; 41; 2 ] in
  for i = 2 to 10 do
    for j = i to 100_000 do
      let thing = lotto_select i j in
      List.iter (Printf.printf "%d ") thing;
      print_newline ();
      assert (test thing i j)
    done
  done
;;
