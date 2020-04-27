module Map = Nation.Map
module Mapx = Mapx.Make(Map)
module Set = Nation.Set

module Range = struct
  let low = 0, 10
  let med = 10, 20
  let high = 20, 30

  let mnp = function
    | Nation.Tulron
    | Nation.Sodistan -> high
    | Nation.Hekatium
    | Nation.Numendor -> low
    | Nation.Clan -> med

  let sup =
    let f = Range.Int.add 10 in
    function
    | Nation.Tulron
    | Nation.Sodistan -> f low
    | Nation.Hekatium
    | Nation.Numendor -> f high
    | Nation.Clan -> f med
end

let is_chosen s kind =
  State.nation s
  |> Nation.chosen
  |> Set.mem kind

let is_empty k map =
  Map.find k map = Resource.empty

let sum =
  Mapx.foldv Resource.(++) Resource.empty

module Chance = struct
  let cap_of s kind =
    if State.nation s |> Nation.has_trade kind
    then 90 else 80

  let add_trade s kind =
    State.nation s
    |> Nation.has_trade kind
    |> Number.if_ok 5
    |> (+)

  let add_base s support kind =
    if is_chosen s kind && is_empty kind support
    then Number.sub_by 10
    else Number.add_if_ptv 10

  let f s support kind chance = chance
    |> add_base s support kind
    |> add_trade s kind
    |> min (cap_of s kind)

  let from s support =
    State.nation s
    |> Nation.chances
    |> Map.mapi (f s support)
end

module Roll = struct
  let roll_res kind =
    let mnp = Dice.range (Range.mnp kind) in
    let sup = Dice.range (Range.sup kind) in
    Resource.make ~mnp ~sup ()

  let to_res s kind chance =
    let chance' = Bonus.support_chance s chance in
    if is_chosen s kind && Dice.percent chance'
    then roll_res kind |> Bonus.support s kind
    else Resource.empty

  let from s =
    State.nation s
    |> Nation.chances
    |> Map.mapi (to_res s)
end

type t =
  { chances : Nation.chances
  ; resources : Nation.resources
  }

let apply t s = s
  |> State.resource_add (sum t.resources)
  |> State.nation_map (fun nats -> nats
    |> Nation.chances_set t.chances
    |> Nation.support_set t.resources
  )

let make s =
  let map = Roll.from s in
  { chances = Chance.from s map
  ; resources = map
  }

let chances t = t.chances
let resources t = t.resources

let chances_init s =
  let f acc k =
    let chance = Chance.cap_of s k in
    Map.add k chance acc
  in
  List.fold_left f Map.empty Nation.kinds
