open Convert
open Game
open Printf

let to_str a b =
  let f = function
    | ((n, Some x), (_, Some y)) -> sprintf "%s -> %s -> %s" (nation2str n) (res2str x) (res2str y)
    | ((n, _), _) -> sprintf "%s -> none" (nation2str n)
  in
  List.combine a b |> List.map f |> String.concat "\n"

let rec run i ldr res =
  let a = Nation.(support_of_list t_list) in
  let b = Nation.apply_bonus a res in
  printf "\n\n%s" (to_str a b);
  if i > 0 then run (pred i) ldr res

let rec roll () =
  let ldr = Leader.make () in
  if Leader.(type_of ldr = Warrior)
  then roll ()
  else
    if Leader.(cha_of ldr < 12)
    then roll ()
    else ldr

let _ =
  Random.self_init ();
  let ldr = roll () in
  let res = Leader.res_bonus_of ldr in
  printf "leader: %s\nbonus: %s" (leader2str ldr) (res2str res);
  run 5 ldr res;
  print_newline ()
