open Resource

type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera

let t_list = [Arnerula; Elanis; Lerota; Sekrefir; Sitera]

let roll = Dice.between

let rand a b =
  let n = roll a b in
  if Random.bool ()
  then Manpwr n
  else Supply n

let blessing_of = function
  | Arnerula -> empty <+ rand 0 30
  | Elanis -> empty <+ Manpwr (roll 10 20)
  | Lerota -> empty
  | Sekrefir -> empty <+ Manpwr 5 <+ Supply 10
  | Sitera -> empty <+ Supply (roll 10 20)

let boosted_of = function
  | Arnerula -> empty <+ rand 0 50
  | Elanis as x -> blessing_of x <+ Manpwr 10
  | Lerota -> empty
  | Sekrefir as x -> blessing_of x <+ Manpwr 10 <+ Supply 10
  | Sitera as x -> blessing_of x <+ Supply 10

let starting deity =
  blessing_of deity
  <+ Manpwr (roll 20 40)
  <+ Supply (roll 60 120)
