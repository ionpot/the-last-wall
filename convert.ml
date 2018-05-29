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
  Leader.(sprintf "level %d %s" (level_of ldr) (type_of ldr |> ltype2str))

let manp2str mp =
  sprintf "(%d mnp)" mp

let res2str res =
  Resource.(sprintf "(%d mnp, %d sup)" (manp res) (supp res))

let party2str pt =
  Enemy.(sprintf "%d %s" (count_of pt) (type_of pt |> enemy2str))

let enemies2str ls =
  List.map party2str ls
  |> String.concat ", "

let yn2bool ch =
  ch = 'y'
