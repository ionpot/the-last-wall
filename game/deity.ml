type t = Arnerula | Elanis | Lerota | Sekrefir | Sitera

module Set = Set.Make(struct
  type nonrec t = t
  let compare = compare
end)

let set = Set.of_list [Arnerula; Elanis; Lerota; Sekrefir; Sitera]

let empty = Set.choose set

module Roll = struct
  let roll = Dice.between

  let random a b =
    let n = roll a b in
    if Dice.yes ()
    then Resource.make ~mnp:n ()
    else Resource.make ~sup:n ()

  let blessing = function
    | Arnerula -> random 0 30
    | Elanis -> Resource.make ~mnp:(roll 10 20) ()
    | Lerota -> Resource.empty
    | Sekrefir -> Resource.make ~mnp:5 ~sup:10 ()
    | Sitera -> Resource.make ~sup:(roll 10 20) ()

  let boosted = function
    | Arnerula -> random 10 40
    | Elanis as x -> blessing x |> Resource.add ~mnp:10
    | Lerota -> Resource.empty
    | Sekrefir as x -> blessing x |> Resource.add ~mnp:10 ~sup:10
    | Sitera as x -> blessing x |> Resource.add ~sup:10

  let starting deity =
    blessing deity
    |> Resource.add ~mnp:(roll 20 40) ~sup:(roll 60 120)
end
