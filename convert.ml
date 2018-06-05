open Game
open Printf

let char2deity = function
  | '1' -> Deity.Elanis
  | '2' -> Deity.Sitera
  | '3' -> Deity.Sekrefir
  | _ -> Deity.None

let deity2char = function
  | Deity.None -> '0'
  | Deity.Elanis -> '1'
  | Deity.Sitera -> '2'
  | Deity.Sekrefir -> '3'

let deity2str = function
  | Deity.None -> "none"
  | Deity.Elanis -> "elanis"
  | Deity.Sitera -> "sitera"
  | Deity.Sekrefir -> "sekrefir"

let enemy2str = function
  | Enemy.Skeleton -> "skeleton"
  | Enemy.Orc -> "orc"
  | Enemy.Demon -> "demon"

let ltype2str = function
  | Leader.Aristocrat -> "aristocrat"
  | Leader.Expert -> "expert"
  | Leader.Warrior -> "warrior"

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

let line2nats chosen str =
  if str = ""
  then chosen
  else
    Nation.t_list
    |> List.filter (fun nat -> String.contains str (nation2char nat))

let leader2str ldr =
  let lv, cha = Leader.(level_of ldr, cha_of ldr) in
  let ty = Leader.type_of ldr |> ltype2str in
  sprintf "level %d %s (cha %d)" lv ty cha

let manp2str mp =
  sprintf "%d mnp" mp

let sup2str sp =
  sprintf "%d sup" sp

let report2str ls =
  let f (count, e) = sprintf "%d %s" count (enemy2str e) in
  List.map f ls
  |> String.concat ", "

let sum2str (count, enemies) =
  let ls = List.map enemy2str enemies in
  let str = String.concat ", " ls in
  (count, str)

let res2str res =
  let m, s = Resource.(manp res, supp res) in
  sprintf "%s, %s" (manp2str m) (sup2str s)

let party2str pt =
  Enemy.(sprintf "%d %s" (count_of pt) (type_of pt |> enemy2str))

let enemies2str ls =
  List.map party2str ls
  |> String.concat ", "

let yn2bool ch =
  ch = 'y'
