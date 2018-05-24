let build f =
  List.fold_left f []

let pick_first i =
  let f ls n =
    if List.length ls < i
    then n :: ls
    else ls
  in
  build f

(* partially applied functions cannot be generalised *)
let undupe ls =
  let f acc x =
    if List.mem x acc
    then acc
    else x :: acc
  in
  build f ls
