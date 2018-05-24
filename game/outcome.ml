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

let support () =
  let f () =
    let a = Dice.deviate 10 5 in
    let b = Dice.deviate 5 5 in
    let (m, s) =
      if Random.bool ()
      then (a, b)
      else (b, a)
    in
    make (Manpwr m) <+ Supply s
  in
  if Dice.chance 0.8
  then Some (f ())
  else None

let upkeep mp =
  make (Supply mp)
