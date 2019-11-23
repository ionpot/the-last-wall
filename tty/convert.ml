open Game
open Printf

let brackets = sprintf "[%s]"
let clean = List.filter (fun str -> str <> "")
let commas = String.concat ", "
let if_empty a b = if b = "" then a else b
let map_commas f ls = List.map f ls |> clean |> commas
let spaces = String.concat " "
let sort_str = List.sort String.compare
let str2none = if_empty "none"

let str2chars str =
  let rec next i ls =
    let j = pred i in
    if j < 0 then ls else next j (str.[j] :: ls)
  in
  next (String.length str) []

let percent2intstr x =
  sprintf "%d%%" x

let percent2str x =
  sprintf "%.2f%%" (x *. 100.)

let power2str x =
  sprintf "%.3f" x

let ichar2int ch =
  int_of_char ch - 49

let int2ichar i =
  char_of_int (49 + i)

let manp2str mp =
  sprintf "%d mnp" mp

let sup2str sp =
  sprintf "%d sup" sp

let work2str wp =
  sprintf "%d wrp" wp

let res2str res =
  Resource.([supp_of res, sup2str; manp_of res, manp2str])
  |> List.filter (fun (n, _) -> n > 0)
  |> List.map (fun (n, f) -> f n)
  |> commas

let res2nothing res =
  if res = Resource.empty
  then "nothing"
  else res2str res

let ldr2kind = function
  | Leader.Aristocrat -> "aristocrat"
  | Leader.Engineer -> "engineer"
  | Leader.Merchant -> "merchant"

let ldr2gender ldr =
  match Leader.gender_of ldr with
  | Leader.Male -> "♂"
  | Leader.Female -> "♀"

let ldr2first ldr = Leader.name_of ldr |> Name.first
let ldr2name ldr = Leader.name_of ldr |> Name.full

let ldr2status ldr =
  let level = Leader.level_of ldr in
  let kind = Leader.kind_of ldr |> ldr2kind in
  let cha = Leader.cha_of ldr in
  sprintf "level %d %s (cha %d)" level kind cha

let ldr2full ldr =
  let gender = ldr2gender ldr in
  let name = ldr2name ldr in
  let status = ldr2status ldr in
  sprintf "%s %s, %s" gender name status

let nation2str = function
  | Nation.Clan -> "clan"
  | Nation.Hekatium -> "hekatium"
  | Nation.Numendor -> "numendor"
  | Nation.Sodistan -> "sodistan"
  | Nation.Tulron -> "tulron"

let trade2str nation =
  sprintf "trading with %s" (nation2str nation)

let nation_suffix = function
  | Some nation -> sprintf " (%s)" (nation2str nation)
  | None -> ""

let bld2str nat = function
  | Build.Arena -> "arena"
  | Build.Barracks ->
      Nation.barracks nat
      |> nation_suffix
      |> sprintf "barracks%s"
  | Build.Engrs -> "engineers guild"
  | Build.Fort -> "fort"
  | Build.Foundry -> "iron foundry"
  | Build.Guesthouse -> "guesthouse"
  | Build.Market -> "market"
  | Build.Mausoleum ldr ->
      "mausoleum for " ^ ldr2name ldr
  | Build.Observatory -> "observatory"
  | Build.Sawmill -> "sawmill"
  | Build.Stable -> "stable"
  | Build.Tavern -> "tavern"
  | Build.Temple -> "temple"
  | Build.Trade ->
      Nation.trade nat
      |> nation_suffix
      |> sprintf "trade guild%s"

let bld_n2str nat (kind, n) =
  let str = bld2str nat kind in
  if n < 1 then ""
  else if n = 1 then str
  else sprintf "%s (%d)" str n

let bld_pairs2str nat ls =
  List.map (bld_n2str nat) ls
  |> sort_str
  |> commas

let bld_ls2str nat ls =
  Listx.group ls
  |> bld_pairs2str nat

let bld_map2str nat map =
  Build.Map.bindings map
  |> bld_pairs2str nat

let bld_q2str nat ls =
  ls
  |> List.rev_map (fun (kind, cost) ->
      sprintf "%s (%s)" (bld2str nat kind) (res2str cost))
  |> commas

let facs2bool map =
  not @@ Build.Map.is_empty map

let facs2clean map =
  Build.Map.filter (fun _ -> (<>) Resource.empty) map

let facs2str nat map =
  let f k r acc =
    sprintf "%s (%s)" (bld2str nat k) (res2str r) :: acc
  in
  Build.Map.fold f map []
  |> sort_str |> commas

let deity2str = function
  | Deity.Arnerula -> "arnerula"
  | Deity.Elanis -> "elanis"
  | Deity.Lerota -> "lerota"
  | Deity.Sitera -> "sitera"
  | Deity.Sekrefir -> "sekrefir"

let deity_text = function
  | Deity.Arnerula -> "deity of sacred fire, ambition and rage"
  | Deity.Elanis -> "lady of triumph, protector of humans"
  | Deity.Lerota -> "lady of life and death, present leader of chaos"
  | Deity.Sitera -> "mother earth, spring of all living"
  | Deity.Sekrefir -> "leader of gods, envoy of order and justice"

let unit_order =
  Units.([Men; Veteran; Merc; Berserker; Cavalry; Knight; Ranger; Templar; Dervish; Ballista; Skeleton; Orc; Demon; Harpy; Cyclops])

let unit_cmp = Listx.compare unit_order

let unit2str = function
  | Units.Ballista -> "ballista"
  | Units.Berserker -> "berserker"
  | Units.Cavalry -> "cavalry"
  | Units.Cyclops -> "cyclops"
  | Units.Demon -> "demon"
  | Units.Dervish -> "dervish"
  | Units.Dullahan -> "dullahan"
  | Units.Harcher -> "horse archer"
  | Units.Harpy -> "harpy"
  | Units.Knight -> "knight"
  | Units.Men -> "men"
  | Units.Merc -> "merc"
  | Units.Orc -> "orc"
  | Units.Ranger -> "ranger"
  | Units.Skeleton -> "skeleton"
  | Units.Templar -> "templar"
  | Units.Veteran -> "veteran"

let unit_n2str n kind =
  if n > 0
  then sprintf "%d %s" n (unit2str kind)
  else ""

let party2str (kind, n) =
  unit_n2str n kind

let unit_ls2str kinds =
  List.sort unit_cmp kinds
  |> map_commas unit2str

let unit_pairs2str ls =
  List.sort (fun (a, _) (b, _) -> unit_cmp a b) ls
  |> map_commas party2str

let units2bool t =
  Units.is_empty t |> not

let units2str t =
  unit_pairs2str (Units.report t)

let units2mnpstr base t =
  Power.of_units t base
  |> truncate
  |> manp2str
  |> sprintf "%s -> %s" (units2str t |> if_empty "no units")

let units2work base t =
  let u = Units.(filter Attr.can_build) t in
  Power.of_units u base
  |> truncate

let report_type2str = function
  | Attack.Accurate ls -> unit_pairs2str ls
  | Attack.Vague (count, kinds) ->
      let str = Units.Set.elements kinds |> unit_ls2str in
      if count > 0
      then sprintf "about %d (%s)" count str
      else ""

let report2str rp =
  report_type2str rp
  |> if_empty "no enemies"

let result2outcome r =
  Dist.outcome r |> units2str

let result2remaining r =
  Dist.remaining r |> units2str

let result2stats r =
  Dist.(["absorbed", absorbed r; "healed", healed r; "reflected", reflected r])
  |> List.filter (fun (_, n) -> n > 0.)
  |> List.map (fun (s, n) -> sprintf "%s %s" (power2str n) s)
  |> commas

let barrage2str x =
  units2str Units.(make x Orc)

let starve2bool (deserted, starved) =
  not (Units.is_empty starved && Units.is_empty deserted)

let mishap2str = function
  | Mishap.Comet -> "comet sighted"
  | Mishap.Disease -> "disease outbreak"
  | Mishap.Tavern -> "tavern burnt down"

let month2str = function
  | Month.Jan -> "january"
  | Month.Feb -> "february"
  | Month.Mar -> "march"
  | Month.Apr -> "april"
  | Month.May -> "may"
  | Month.Jun -> "june"
  | Month.Jul -> "july"
  | Month.Aug -> "august"
  | Month.Sep -> "september"
  | Month.Oct -> "october"
  | Month.Nov -> "november"
  | Month.Dec -> "december"

let degree2str = function
  | Weather.Light -> "light"
  | Weather.Heavy -> "heavy"

let weather2str = function
  | Weather.Breeze -> "breeze"
  | Weather.Clear -> "clear sky"
  | Weather.Cloudy -> "cloudy"
  | Weather.Fog -> "fog"
  | Weather.Heat -> "scorching heat"
  | Weather.Wind -> "strong wind"
  | Weather.Rain d -> sprintf "%s rain" (degree2str d)
  | Weather.Snow d -> sprintf "%s snow" (degree2str d)

let turn2str (turn, m, w) =
  sprintf "turn %d (%s, %s)" turn (month2str m) (weather2str w)
