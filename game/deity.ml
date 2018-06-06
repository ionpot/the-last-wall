open Resource

type t = Arnerula | Elanis | Sekrefir | Sitera | None

let t_list = [Arnerula; Elanis; Sekrefir; Sitera; None]

let roll = Dice.between

let rand a b =
  let n = roll a b in
  if Random.bool ()
  then Manpwr n
  else Supply n

let blessing_of deity =
  let res = make Empty in
  match deity with
  | Arnerula -> res <+ rand 0 30
  | Elanis -> res <+ Manpwr (roll 10 20)
  | Sitera -> res <+ Supply (roll 10 20)
  | Sekrefir -> res <+ Manpwr (roll 0 10) <+ Supply (roll 0 10)
  | None -> res

let starting deity =
  blessing_of deity
    <+ Manpwr (roll 10 30)
    <+ Supply (roll 150 300)
