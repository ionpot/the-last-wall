type t =
  | Attack
  | Blessing
  | Deity
  | Nation
  | Over
  | Starting
  | Support
  | Turn
  | Upkeep

let first_turn =
  [Deity; Starting; Nation; Blessing; Support]

let other_turns =
  [Upkeep; Blessing; Nation; Support; Attack]

let first = List.hd first_turn

let rec next_in ls st =
  if st = Over
  then Over
  else match ls with
  | x :: y :: rest ->
      if x = st
      then y
      else next_in (y :: rest) st
  | _ -> Turn
