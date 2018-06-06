open Resource

type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera | None

let t_list = [Arnerula; Elanis; Lerota; Sekrefir; Sitera; None]

let roll = Dice.between

let rand a b =
  let n = roll a b in
  if Random.bool ()
  then Manpwr n
  else Supply n

let resource_of =
  let res = make Empty in
  function
  | Arnerula -> res <+ rand 0 30
  | Elanis -> res <+ Manpwr (roll 10 20)
  | Sitera -> res <+ Supply (roll 10 20)
  | Sekrefir -> res <+ Manpwr (roll 0 10) <+ Supply (roll 0 10)
  | Lerota
  | None -> res

let blessing_of deity =
  let res = resource_of deity in
  if not (empty res)
  then Some res
  else None

let starting deity =
  resource_of deity
  <+ Manpwr (roll 10 30)
  <+ Supply (roll 150 300)
