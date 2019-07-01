type charisma = Defs.count
type gender = Female | Male
type kind = Aristocrat | Expert | Warrior
type level = Defs.count

type t =
  { cha : charisma;
    died : Defs.turn;
    gender : gender;
    kind : kind;
    level : level;
    name : Name.t;
    noble : bool;
    xp : Defs.count
  }

let empty =
  { cha = 0;
    died = 0;
    gender = Female;
    kind = Aristocrat;
    level = 0;
    name = Name.empty;
    noble = true;
    xp = 0
  }

let kinds = [Aristocrat; Expert; Warrior]
let respawn_time = 2 (* turns *)

let def_bonus_of cha = function
  | Aristocrat
  | Expert -> 0.0
  | Warrior -> 0.01 *. float cha

let mod_of cha =
  (cha - 10) / 2

let resource_of cha = function
  | Aristocrat -> Resource.of_manp (2 * cha)
  | Expert -> Resource.of_supp (2 * cha)
  | Warrior -> Resource.empty

let gender_of t = t.gender
let is_alive t = t.died = 0
let is_dead t = t.died > 0
let can_respawn turn t =
  is_dead t && t.died + respawn_time <= turn
let is_noble t = t.noble
let kind_of t = t.kind
let level_of t = t.level + t.xp / 2
let name_of t = t.name
let cha_of t = t.cha + level_of t / 4
let cha_mod_of t = t |> cha_of |> mod_of
let lvup t = is_alive t && t.xp mod 2 = 0
let victories t = t.xp

let base_defense t =
  let lv = level_of t in
  0.1 +. (0.01 *. float lv)

let defense_of t =
  let base = base_defense t in
  let cha = cha_mod_of t in
  let bonus = kind_of t |> def_bonus_of cha in
  base +. bonus

let res_bonus_of t =
  let cha = cha_mod_of t in
  let kind = kind_of t in
  resource_of cha kind

let died turn t =
  { t with died = turn }

let won t =
  { t with xp = t.xp + 1 }

module Roll (Dice : Dice.S) = struct
  module Name = Name.Roll(Dice)

  let death t =
    if is_alive t then Dice.chance 0.05 else false

  let name t =
    let first = Names.(if t.gender = Female then female else male) in
    let house = if t.noble then Names.house else [] in
    { t with name = Name.from first house Names.title t.name }

  let noble = function
    | Aristocrat -> true
    | Expert -> Dice.chance 0.4
    | Warrior -> Dice.chance 0.2

  let from kind =
    { empty with
      cha = Dice.between 10 15;
      gender = if Dice.yes () then Male else Female;
      kind;
      level = Dice.between 3 5;
      noble = noble kind
    } |> name

  let random () =
    let a, kinds' = Dice.pop kinds in
    let b = Dice.pick kinds' in
    [from a; from b]
end
