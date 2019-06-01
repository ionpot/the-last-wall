let apply_to ls x =
  List.iter (fun f -> f x) ls

let build f =
  List.fold_left f []

let count x xs =
  List.filter ((=) x) xs
  |> List.length

let dedupe_if f ls =
  let f x acc =
    if f x && List.mem x acc
    then acc
    else x :: acc
  in
  List.fold_right f ls []

let dedupe ls =
  dedupe_if (fun _ -> true) ls

let discard f ls =
  List.filter (fun x -> not (f x)) ls

let filteri f ls =
  let rec next i = function
    | [] -> []
    | x :: rest ->
        let j = succ i in
        if f i x
        then x :: next j rest
        else next j rest
  in
  next 0 ls

let group ls =
  let one x = (1, x) in
  let bump (n, x) = succ n, x in
  let is x (_, y) = x = y in
  build (fun out mem ->
    let found, rest = List.partition (is mem) out in
    if found = [] then one mem :: rest
    else List.map bump found @ rest) ls

let index_of a ls =
  let rec f i = function
    | [] -> -1
    | x :: xs -> if a = x then i else f (succ i) xs
  in f 0 ls

let compare ls a b =
  let i = index_of a ls in
  let j = index_of b ls in
  if i < j then -1
  else if i > j then 1
  else 0

let in_both a b =
  List.filter (fun x -> List.mem x a) b

let rec min_of = function
  | [] -> 0
  | x :: [] -> x
  | x :: xs -> min x (min_of xs)

let rec pick num probs = function
  | [] -> assert false
  | [x] -> x
  | x :: xs ->
      let num' = num -. List.hd probs in
      if num' > 0. then pick num' (List.tl probs) xs else x

let pick_first i =
  filteri (fun j _ -> j < i)

let rm x ls =
  List.filter ((<>) x) ls

let rm_from ls xs =
  List.fold_left (fun ls' x -> rm x ls') ls xs

let rec slice_from f = function
  | [] -> []
  | x :: xs as ls -> if f x then ls else slice_from f xs

let sum ls =
  List.fold_left (+) 0 ls

let sumf ls =
  List.fold_left (+.) 0. ls

let swap_nth i x ls =
  List.mapi (fun j y -> if i = j then x else y) ls

let unfold x f =
  let rec next = function
    | None -> []
    | Some (a, b) -> b :: next (f a)
  in
  next (f x)

let unfold_with x f =
  let rec next a = function
    | None -> a, []
    | Some (a, b) ->
        let a', ls = next a (f a) in
        a', b :: ls
  in
  next x (f x)

let fn_unfold_list f (acc, ls) =
  match ls with
  | [] -> None
  | x :: xs ->
    let a, b = f acc x in
    Some ((a, xs), b)

let map_with f init ls =
  let f' = fn_unfold_list f in
  unfold (init, ls) f'

let fold_map f init ls =
  let f' = fn_unfold_list f in
  let (final, _), ls' = unfold_with (init, ls) f' in
  final, ls'
