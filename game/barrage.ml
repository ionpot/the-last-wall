type reason = Leader | Weather
type status = Available | Disabled of reason

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

let set_choice choice t =
  { t with choice }

let set_trained trained t =
  { t with trained }
