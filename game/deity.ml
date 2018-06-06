open Resource

type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera

let default = Sekrefir

let t_list = [Arnerula; Elanis; Lerota; Sekrefir; Sitera]

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
  | Lerota -> res
  | Sekrefir -> res <+ Manpwr (roll 0 10) <+ Supply (roll 0 10)
  | Sitera -> res <+ Supply (roll 10 20)

let blessing_of deity =
  let res = resource_of deity in
  if empty res
  then None
  else Some res

let starting deity =
  resource_of deity
  <+ Manpwr (roll 10 30)
  <+ Supply (roll 150 300)
