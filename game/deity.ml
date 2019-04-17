open Resource

type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera

let empty = Arnerula

let roll = Dice.between

let rand a b =
  let n = roll a b in
  if Random.bool ()
  then Manpwr n
  else Supply n

let blessing_of = function
  | Arnerula -> Resource.empty <+ rand 0 30
  | Elanis -> Resource.empty <+ Manpwr (roll 10 20)
  | Lerota -> Resource.empty
  | Sekrefir -> Resource.empty <+ Manpwr 5 <+ Supply 10
  | Sitera -> Resource.empty <+ Supply (roll 10 20)

let boosted_of = function
  | Arnerula -> Resource.empty <+ rand 0 50
  | Elanis as x -> blessing_of x <+ Manpwr 10
  | Lerota -> Resource.empty
  | Sekrefir as x -> blessing_of x <+ Manpwr 10 <+ Supply 10
  | Sitera as x -> blessing_of x <+ Supply 10

let starting deity =
  blessing_of deity
  <+ Manpwr (roll 20 40)
  <+ Supply (roll 60 120)
