let apply_to ls x =
  List.iter (fun f -> f x) ls

let build f =
  List.fold_left f []

let count x xs =
  List.filter ((=) x) xs
  |> List.length

let discard f ls =
  List.filter (fun x -> not (f x)) ls

let in_both a b =
  List.filter (fun x -> List.mem x a) b

let rec min_of = function
  | [] -> 0
  | x :: [] -> x
  | x :: xs -> min x (min_of xs)

let pick_first i =
  let f ls n =
    if List.length ls < i
    then n :: ls
    else ls
  in
  build f

let pick_from ls =
  List.length ls |> Random.int |> List.nth ls

let rm x ls =
  List.filter ((<>) x) ls

let rec slice_from f = function
  | [] -> []
  | x :: xs as ls -> if f x then ls else slice_from f xs

let sumf ls =
  List.fold_left (+.) 0. ls

let swap_nth i x ls =
  List.mapi (fun j y -> if i = j then x else y) ls

let undupe ls =
  let f acc x =
    if List.mem x acc
    then acc
    else x :: acc
  in
  build f ls

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
