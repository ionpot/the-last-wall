open Defs
open Resource

type resource = t

let blessing d =
  let r n = Dice.deviate n 5 in
  let res = make Empty in
  match d with
  | Elanis -> res <+ Manpwr (r 15)
  | Sitera -> res <+ Supply (r 15)
  | Sekrefir -> res <+ Manpwr (r 5) <+ Supply (r 5)
  | NoDeity -> res

let starting d =
  blessing d
    <+ Manpwr (Dice.between 10 30)
    <+ Supply (Dice.between 90 180)

let upkeep mp =
  make (Supply mp)
