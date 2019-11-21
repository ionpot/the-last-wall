type reason = Leader | Weather
type status = Available | Disabled of reason

let base_coefficient = 0.05

type t =
  { choice : bool
  ; trained : bool
  }

let empty =
  { choice = false
  ; trained = false
  }

let is_chosen t = t.choice
let is_trained t = t.trained

let coefficient t =
  if is_trained t then 0.1 else base_coefficient

let set_choice choice t =
  { t with choice }

let set_trained trained t =
  { t with trained }
