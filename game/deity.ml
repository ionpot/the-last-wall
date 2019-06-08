open Resource

type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera

let empty = Arnerula

let list = [Arnerula; Elanis; Lerota; Sekrefir; Sitera]

module Roll (Dice : Dice.S) = struct
  let roll = Dice.between

  let rand a b =
    let n = roll a b in
    if Dice.yes ()
    then Manpwr n
    else Supply n

  let blessing = function
    | Arnerula -> Resource.empty <+ rand 20 30
    | Elanis -> Resource.empty <+ Manpwr (roll 10 20)
    | Lerota -> Resource.empty
    | Sekrefir -> Resource.empty <+ Manpwr 5 <+ Supply 10
    | Sitera -> Resource.empty <+ Supply (roll 10 20)

  let boosted = function
    | Arnerula -> Resource.empty <+ rand 30 40
    | Elanis as x -> blessing x <+ Manpwr 10
    | Lerota -> Resource.empty
    | Sekrefir as x -> blessing x <+ Manpwr 10 <+ Supply 10
    | Sitera as x -> blessing x <+ Supply 10

  let starting deity =
    blessing deity
    <+ Manpwr (roll 20 40)
    <+ Supply (roll 60 120)
end
