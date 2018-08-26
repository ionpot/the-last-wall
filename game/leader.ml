type ltype = Aristocrat | Expert | Warrior
type level = int
type loss = Resource.t
type charisma = int

type t =
  { ltype : ltype;
    level : level;
    cha_base : charisma;
    cha_extra : charisma;
    xp : int ref
  }

let ltypes = [Aristocrat; Expert; Warrior]

let mod_of cha =
  (cha - 10) / 2

let defense_of cha = function
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
    xp = ref 0
  }

let random () =
  make (Listx.pick_from ltypes)

let lives () =
  Dice.chance 0.95

let won t = incr t.xp
let type_of t = t.ltype
let level_of t = t.level
let cha_of t = t.cha_base + t.cha_extra
let can_lvup t = !(t.xp) > 1

let lvup t =
  let xp = !(t.xp) in
  let lv = t.level + (xp / 2) in
  { t with
    level = lv;
    cha_extra = lv / 4;
    xp = ref (xp mod 2)
  }

let cha_mod_of t =
  t |> cha_of |> mod_of

let base_defense t =
  let lv = level_of t in
  0.1 +. (0.01 *. float lv)

let mitigate loss t =
  let base = base_defense t in
  let cha = cha_mod_of t in
  let extra = type_of t |> defense_of cha in
  let x = base +. extra in
  let y = Resource.manp_of loss in
  truncate (x *. float y)
  |> Resource.of_manp

let res_bonus_of t =
  let cha = cha_mod_of t in
  let typ = type_of t in
  resource_of cha typ
