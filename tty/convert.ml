open Game
open Printf

let brackets str =
  sprintf "[%s]" str

let chars_of str =
  let rec f i ls =
    if i < 0 then ls else f (pred i) (str.[i] :: ls)
  in
  f (String.length str |> pred) []

let commas ls =
  String.concat ", " ls

let str2char = function
  | "" -> '0'
  | x -> String.get x 0

let char2bool ch =
  ch = 'y'

  (*
let manp2str mp =
  sprintf "%d mnp" mp

let sup2str sp =
  sprintf "%d sup" sp

let cav2str cav =
  sprintf "%d cav" cav

let res2str res =
  let m, s = Resource.(manp_of res, supp_of res) in
  sprintf "%s, %s" (manp2str m) (sup2str s)

let bld2char = function
  | Building.Fort -> '1'
  | Building.Market -> '2'
  | Building.Stable -> '3'
  | Building.Tavern -> '4'
  | Building.Temple -> '5'

let bld2str = function
  | Building.Fort -> "fort"
  | Building.Market -> "market"
  | Building.Stable -> "stable"
  | Building.Tavern -> "tavern"
  | Building.Temple -> "temple"

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

let buildings2str bs =
  List.map bld2str bs
  |> commas

let char2deity = function
  | '1' -> Deity.Arnerula
  | '2' -> Deity.Elanis
  | '3' -> Deity.Lerota
  | '4' -> Deity.Sitera
  | _ -> Deity.Sekrefir

let deity2char = function
  | Deity.Arnerula -> '1'
  | Deity.Elanis -> '2'
  | Deity.Lerota -> '3'
  | Deity.Sitera -> '4'
  | Deity.Sekrefir -> '5'

let deity2str = function
  | Deity.Arnerula -> "arnerula"
  | Deity.Elanis -> "elanis"
  | Deity.Lerota -> "lerota"
  | Deity.Sitera -> "sitera"
  | Deity.Sekrefir -> "sekrefir"

let enemy2str = function
  | Enemy.Skeleton -> "skeleton"
  | Enemy.Orc -> "orc"
  | Enemy.Demon -> "demon"

let char2ltype = function
  | '1' -> Leader.Aristocrat
  | '2' -> Leader.Expert
  | _ -> Leader.Warrior

let ltype2char = function
  | Leader.Aristocrat -> '1'
  | Leader.Expert -> '2'
  | Leader.Warrior -> '3'

let ltype2str = function
  | Leader.Aristocrat -> "aristocrat"
  | Leader.Expert -> "expert"
  | Leader.Warrior -> "warrior"

let leader2str ldr =
  let lv, cha = Leader.(level_of ldr, cha_of ldr) in
  let ty = Leader.type_of ldr |> ltype2str in
  sprintf "level %d %s (cha %d)" lv ty cha

let nation2char = function
  | Nation.Tulron -> '1'
  | Nation.Sodistan -> '2'
  | Nation.Hekatium -> '3'
  | Nation.Numendor -> '4'
  | Nation.Clan -> '5'

let nation2str = function
  | Nation.Tulron -> "tulron"
  | Nation.Sodistan -> "sodistan"
  | Nation.Hekatium -> "hekatium"
  | Nation.Numendor -> "numendor"
  | Nation.Clan -> "clan"

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
