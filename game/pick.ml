let invalid ls =
  List.map string_of_int ls
  |> String.concat ", "
  |> Printf.sprintf "invalid list [%s]"
  |> failwith

let probs_of powers =
  let sum = List.fold_left (+.) 0. powers in
  let ratio = 1. /. sum in
  List.map (( *.) ratio) powers

let pick probs =
  let rec next input cap =
    if cap <= 0. then input
    else 
  in next

let units cap ls =
  let counts, powers = List.split ls in
  let probs = probs_of powers in
  let input = List.map2 (fun c p -> float c *. p, 0.) counts powers in
  pick probs input cap
  |> List.map snd
  |> List.map2 (fun p c -> c /. p) powers
  |> List.map truncate
