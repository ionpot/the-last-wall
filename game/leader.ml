type charisma = int
type kind = Aristocrat | Expert | Warrior
type level = int

type t =
  { cha_base : charisma;
    cha_extra : charisma;
    died : Defs.turn;
    kind : kind;
    level : level;
    noble : bool;
    xp : int
  }

let empty =
  { cha_base = 0;
    cha_extra = 0;
    died = 0;
    kind = Aristocrat;
    level = 0;
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

let roll_noble = function
  | Aristocrat -> true
  | Expert -> Dice.chance 0.4
  | Warrior -> Dice.chance 0.2

let make kind =
  let level = Dice.between 3 5 in
  { cha_base = Dice.between 10 15;
    cha_extra = (level / 4);
    died = 0;
    kind;
    level;
    noble = roll_noble kind;
    xp = 0
  }

let random () =
  make (Listx.pick_from kinds)

let can_lvup t = t.xp > 1
let cha_of t = t.cha_base + t.cha_extra
let cha_mod_of t = t |> cha_of |> mod_of
let is_alive t = t.died = 0
let is_dead t = t.died > 0
let can_respawn turn t =
  is_dead t && t.died + respawn_time <= turn
let is_noble t = t.noble
let kind_of t = t.kind
let level_of t = t.level

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

let roll_death t =
  if is_alive t then Dice.chance 0.05 else false

let died turn t =
  { t with died = turn }

let lvup t =
  let level = t.level + (t.xp / 2) in
  { t with
    cha_extra = level / 4;
    level;
    xp = t.xp mod 2
  }

let won t =
  { t with xp = t.xp + 1 }
