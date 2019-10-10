type reason = Leader | Weather
type status = Available | Disabled of reason

type t = bool

let empty = false

let is_chosen t = t

let set_choice choice t = choice
