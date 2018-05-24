open Resource

type t = Elanis | Sekrefir | Sitera | None

let blessing_of deity =
  let r n = Dice.deviate n 5 in
  let res = make Empty in
  match deity with
  | Elanis -> res <+ Manpwr (r 15)
  | Sitera -> res <+ Supply (r 15)
  | Sekrefir -> res <+ Manpwr (r 5) <+ Supply (r 5)
  | None -> res

let starting deity =
  blessing_of deity
    <+ Manpwr (Dice.between 10 30)
    <+ Supply (Dice.between 90 180)
