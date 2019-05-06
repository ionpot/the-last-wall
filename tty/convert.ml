open Game
open Printf

let brackets = sprintf "[%s]"
let commas = String.concat ", "
let map_commas f ls = List.map f ls |> commas
let spaces = String.concat " "
let sort_str = List.sort String.compare

let percent2str x =
  sprintf "%.2f%%" (x *. 100.)

let power2str x =
  sprintf "%.3f" x

let int2ichar i =
  char_of_int (48 + i)

let manp2str mp =
  sprintf "%d mnp" mp

let sup2str sp =
  sprintf "%d sup" sp

let res2str res =
  let m, s = Resource.(manp_of res, supp_of res) in
  sprintf "%s, %s" (manp2str m) (sup2str s)

let ldr2kind = function
  | Leader.Aristocrat -> "aristocrat"
  | Leader.Expert -> "expert"
  | Leader.Warrior -> "warrior"

let ldr2first ldr = Leader.name_of ldr |> Name.first
let ldr2name ldr = Leader.name_of ldr |> Name.full

let ldr2status ldr =
  let level = Leader.level_of ldr in
  let kind = Leader.kind_of ldr |> ldr2kind in
  let cha = Leader.cha_of ldr in
  sprintf "level %d %s (cha %d)" level kind cha

let ldr2full ldr =
  let name = ldr2name ldr in
  let status = ldr2status ldr in
  sprintf "%s, %s" name status

let nation2str = function
  | Nation.Clan -> "clan"
  | Nation.Hekatium -> "hekatium"
  | Nation.Numendor -> "numendor"
  | Nation.Sodistan -> "sodistan"
  | Nation.Tulron -> "tulron"

let trade2str = function
  | Nation.Boost kind -> "extra with " ^ nation2str kind
  | Nation.Certain kind -> "certain with " ^ nation2str kind
  | Nation.NoTrade -> ""

let trade_suffix trade =
  let str = trade2str trade in
  if str = "" then str
  else sprintf " (%s)" str

let bld2str = function
  | Build.Engrs -> "engineers guild"
  | Build.Fort -> "fort"
  | Build.Market -> "market"
  | Build.Mausoleum ldr ->
      "mausoleum for " ^ ldr2name ldr
  | Build.Observatory -> "observatory"
  | Build.Stable -> "stable"
  | Build.Tavern -> "tavern"
  | Build.Temple -> "temple"
  | Build.Trade trade ->
      sprintf "trade guild%s" (trade_suffix trade)

let bld_n2str (n, kind) =
  let str = bld2str kind in
  if n < 1 then ""
  else if n = 1 then str
  else sprintf "%s (%d)" str n

let bld_ls2str ls =
  Listx.group ls
  |> List.map bld_n2str
  |> sort_str
  |> commas

let bld_q2str ls =
  ls
  |> List.rev_map (fun (kind, cost) ->
      sprintf "%s (%s)" (bld2str kind) (res2str cost))
  |> commas

let deity2str = function
  | Deity.Arnerula -> "arnerula"
  | Deity.Elanis -> "elanis"
  | Deity.Lerota -> "lerota"
  | Deity.Sitera -> "sitera"
  | Deity.Sekrefir -> "sekrefir"

let unit2str = function
  | Units.Cavalry -> "cavalry"
  | Units.Demon -> "demon"
  | Units.Dervish -> "dervish"
  | Units.Harpy -> "harpy"
  | Units.Men -> "men"
  | Units.Orc -> "orc"
  | Units.Skeleton -> "skeleton"

let unit_ls2str kinds =
  map_commas unit2str kinds

let party2str (n, kind) =
  if n > 0
  then sprintf "%d %s" n (unit2str kind)
  else ""

let units2str t =
  Units.kinds_of t
  |> List.map (fun k -> Units.count k t, k)
  |> map_commas party2str

let report_type2str = function
  | Attack.Accurate ls -> map_commas party2str ls
  | Attack.Vague (count, kinds) ->
      if count > 0
      then sprintf "about %d (%s)" count (unit_ls2str kinds)
      else ""

let report2str rp =
  let str = report_type2str rp in
  if str = "" then "no enemies" else str

let barrage2str x =
  units2str Units.(make x Orc)

let smite2str x =
  units2str Units.(make x Skeleton)

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
  | Weather.Clear -> "clear sky"
  | Weather.Cloudy -> "cloudy"
  | Weather.Wind -> "strong winds"
  | Weather.Rain d -> sprintf "%s rain" (degree2str d)
  | Weather.Snow d -> sprintf "%s snow" (degree2str d)

let turn2str (turn, m, w) =
  sprintf "%d (%s, %s)" turn (month2str m) (weather2str w)
