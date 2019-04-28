open Game
open Printf

let brackets = sprintf "[%s]"
let commas = String.concat ", "
let spaces = String.concat " "

let int2ichar i =
  char_of_int (48 + i)

let manp2str mp =
  sprintf "%d mnp" mp

let sup2str sp =
  sprintf "%d sup" sp

let res2str res =
  let m, s = Resource.(manp_of res, supp_of res) in
  sprintf "%s, %s" (manp2str m) (sup2str s)

let bld2str = function
  | Build.Engrs -> "engineers guild"
  | Build.Fort -> "fort"
  | Build.Market -> "market"
  | Build.Mausoleum ldr ->
      "mausoleum for " ^ (Leader.name_of ldr |> Name.full)
  | Build.Observatory -> "observatory"
  | Build.Stable -> "stable"
  | Build.Tavern -> "tavern"
  | Build.Temple -> "temple"
  | Build.Trade -> "trade guild"

let deity2str = function
  | Deity.Arnerula -> "arnerula"
  | Deity.Elanis -> "elanis"
  | Deity.Lerota -> "lerota"
  | Deity.Sitera -> "sitera"
  | Deity.Sekrefir -> "sekrefir"

module Leader = struct
  let kind2str = function
    | Leader.Aristocrat -> "aristocrat"
    | Leader.Expert -> "expert"
    | Leader.Warrior -> "warrior"
end

let nation2str = function
  | Nation.Clan -> "clan"
  | Nation.Hekatium -> "hekatium"
  | Nation.Numendor -> "numendor"
  | Nation.Sodistan -> "sodistan"
  | Nation.Tulron -> "tulron"

  (*
let bstat2str b bs =
  let module B = Buildings in
  let count = B.count_of b bs in
  let multi = Building.multiple b in
  let ready = B.is_ready b bs in
  let built = B.built bs in
  let just_n = Listx.count b built in
  let just = just_n > 0 in
  let needs =
    B.in_queue bs
    |> List.filter (fun (x, _) -> x = b)
    |> List.map snd
    |> List.map res2str
    |> List.map (fun str -> sprintf "needs %s" str)
    |> String.concat "; "
  in
  let cost = Building.cost_of b |> res2str in
  let status =
    if just then "just built"
    else if ready then "built"
    else if needs <> "" then needs
    else cost
  in
  let multi_status =
    if just then sprintf "built %d+%d" count just_n
    else if ready then sprintf "built %d" count
    else ""
  in
  if multi
  then [multi_status; cost; needs]
    |> List.filter ((<>) "")
    |> List.map brackets
    |> String.concat " "
  else brackets status

let enemy2str = function
  | Enemy.Skeleton -> "skeleton"
  | Enemy.Orc -> "orc"
  | Enemy.Demon -> "demon"

let leader2str ldr =
  let lv, cha = Leader.(level_of ldr, cha_of ldr) in
  let ty = Leader.type_of ldr |> ltype2str in
  sprintf "level %d %s (cha %d)" lv ty cha

let party2str pt =
  Enemy.(sprintf "%d %s" (count_of pt) (type_of pt |> enemy2str))

let enemies2str ls =
  List.map party2str ls
  |> commas

let report2str ls =
  let str = List.map party2str ls |> commas in
  if str = "" then "no enemies" else str

let status2str (mnp, sup, cav) =
  (if cav > 0 then [cav2str cav] else [])
  |> List.cons (sup2str sup)
  |> List.cons (manp2str mnp)
  |> commas

let sum2str (count, enemies) =
  let str = List.map enemy2str enemies |> commas in
  if count > 0
  then sprintf "about %d (%s)" count str
  else "no enemies"

let units2str men cav =
  let ls = [men, "men"; cav, "cavalry"] in
  List.filter (fun (n, _) -> n > 0) ls
  |> List.map (fun (n, u) -> sprintf "%d %s" n u)
  |> commas

let defense2str men cav ldr =
  let str = units2str men cav in
  match ldr with
  | Some x -> sprintf "%s, with %s" str (leader2str x)
  | None -> str
  *)
