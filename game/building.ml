type t = Fort | Market | Stable | Tavern | Temple

let ready = [Tavern]
let tlist = [Fort; Market; Stable; Tavern; Temple]

let multiple t = t = Stable

let cost_pair_of =
  let open Resource in
  function
  | Fort -> Manpwr 124, Supply 136
  | Market -> Manpwr 48, Supply 53
  | Stable -> Manpwr 49, Supply 54
  | Tavern -> Manpwr 0, Supply 0
  | Temple -> Manpwr 29, Supply 28

let cost_of kind =
  let a, b = cost_pair_of kind in
  Resource.(empty <+ a <+ b)
