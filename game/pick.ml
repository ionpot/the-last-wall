let invalid ls =
  List.map string_of_int ls
  |> String.concat ", "
  |> Printf.sprintf "invalid list [%s]"
  |> failwith

let roll count cap =
  let x = min count cap in
  let y = if x < 1. then x else Random.float x in
  max 0. y

let move_nth i cap ls =
  let ic, oc = List.nth ls i in
  let x = roll ic cap in
  let new_ls = Listx.swap_nth i (ic -. x, oc +. x) ls in
  cap -. x, new_ls

let map cap ls =
  let len = List.length ls in
  let rec next cap ls =
    if cap <= 0. then ls
    else
      let i = Random.int len in
      let new_cap, new_ls = move_nth i cap ls in
      next new_cap new_ls
  in next cap ls

let pick cap pairs =
  let counts, powers = List.split pairs in
  let ls = List.map2 Defs.to_power counts powers in
  if (Listx.sumf ls) <= cap then counts
  else
    List.map (fun x -> x, 0.) ls
    |> map cap
    |> List.map snd
    |> List.map2 (fun p c -> c /. p) powers
    |> List.map truncate

let units cap pairs =
  if cap > 0. then pick cap pairs
  else List.map (fun _ -> 0) pairs
