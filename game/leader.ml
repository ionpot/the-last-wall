type ltype = Aristocrat | Expert | Warrior
type level = int
type loss = int

type t =
  { ltype : ltype;
    level : level;
    xp : int ref;
    mutable died_at : Defs.turn
  }

let ltypes = [Aristocrat; Expert; Warrior]

let make () =
  { ltype = Listx.pick_from ltypes;
    level = Dice.between 3 5;
    xp = ref 0;
    died_at = 0
  }

let lives () =
  Dice.chance 0.95

let alive t =
  t.died_at = 0
let won t = incr t.xp
let died t turn = t.died_at <- turn
let died_at t = t.died_at
let type_of t = t.ltype
let level_of t = t.level
let can_lvup t = !(t.xp) > 1
let lvup t =
  let xp = !(t.xp) in
  { t with
    level = t.level + (xp / 2);
    xp = ref (xp mod 2)
  }
let mitigate loss t =
  let lv = level_of t in
  let x = 0.1 +. (0.01 *. float lv) in
  truncate (x *. float loss)
