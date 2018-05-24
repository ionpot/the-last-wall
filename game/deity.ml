open Resource

type t = Elanis | Sekrefir | Sitera | None

let blessing_of d =
  let r n = Dice.deviate n 5 in
  let res = make Empty in
  match d with
  | Elanis -> res <+ Manpwr (r 15)
  | Sitera -> res <+ Supply (r 15)
  | Sekrefir -> res <+ Manpwr (r 5) <+ Supply (r 5)
  | None -> res
