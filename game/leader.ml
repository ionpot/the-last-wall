type ltype = Aristocrat | Expert | Warrior
type level = int
type charisma = int

type t =
  { ltype : ltype;
    level : level;
    cha_base : charisma;
    cha_extra : charisma;
    xp : int;
    died : Defs.turn
  }

let empty =
  { ltype = Aristocrat;
    level = 0;
    cha_base = 0;
    cha_extra = 0;
    xp = 0;
    died = 0
  }

let ltypes = [Aristocrat; Expert; Warrior]
let respawn_time = 2 (* turns *)

let mod_of cha =
  (cha - 10) / 2

let def_bonus_of cha = function
  | Warrior -> 0.01 *. float cha
  | Aristocrat
  | Expert -> 0.0

let resource_of cha = function
  | Aristocrat -> Resource.of_manp (2 * cha)
  | Expert -> Resource.of_supp (2 * cha)
  | Warrior -> Resource.empty

let make ltype =
  let lv = Dice.between 3 5 in
  { ltype;
    level = lv;
    cha_base = Dice.between 10 15;
    cha_extra = (lv / 4);
    xp = 0;
    died = 0
  }

let random () =
  make (Listx.pick_from ltypes)

let type_of t = t.ltype
let level_of t = t.level
let cha_of t = t.cha_base + t.cha_extra
let can_lvup t = t.xp > 1
let is_alive t = t.died = 0
let is_dead t = t.died > 0
let can_respawn turn t =
  is_dead t && t.died + respawn_time <= turn

let has_died t =
  if is_alive t then Dice.chance 0.05 else false

let cha_mod_of t =
  t |> cha_of |> mod_of

let base_defense t =
  let lv = level_of t in
  0.1 +. (0.01 *. float lv)

let defense_of t =
  let base = base_defense t in
  let cha = cha_mod_of t in
  let bonus = type_of t |> def_bonus_of cha in
  base +. bonus

let res_bonus_of t =
  let cha = cha_mod_of t in
  let typ = type_of t in
  resource_of cha typ

let died turn t =
  { t with died = turn }

let lvup t =
  let lv = t.level + (t.xp / 2) in
  { t with
    level = lv;
    cha_extra = lv / 4;
    xp = t.xp mod 2
  }

let won t =
  { t with xp = t.xp + 1 }
