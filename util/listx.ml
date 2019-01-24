let build f =
  List.fold_left f []

let count x xs =
  List.filter ((=) x) xs
  |> List.length

let discard f ls =
  List.filter (fun x -> not (f x)) ls

let rec first_some = function
  | [] -> None
  | f :: ls ->
      match f () with
      | Some _ as x -> x
      | None -> first_some ls

let rec last_of = function
  | [] -> []
  | x :: [] -> x
  | _ :: xs -> last_of xs

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
  | x :: xs as ls -> if f x then ls else slice_from xs

(* partially applied functions cannot be generalised *)
let undupe ls =
  let f acc x =
    if List.mem x acc
    then acc
    else x :: acc
  in
  build f ls
