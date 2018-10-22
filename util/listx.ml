let build f =
  List.fold_left f []

let count x xs =
  List.filter ((=) x) xs
  |> List.length

let pick_first i =
  let f ls n =
    if List.length ls < i
    then n :: ls
    else ls
  in
  build f

let pick_from ls =
  List.length ls |> Random.int |> List.nth ls

(* partially applied functions cannot be generalised *)
let undupe ls =
  let f acc x =
    if List.mem x acc
    then acc
    else x :: acc
  in
  build f ls
