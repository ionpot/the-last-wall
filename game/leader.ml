type ltype = Aristocrat | Expert | Warrior
type level = int

type t =
  { ltype : ltype;
    level : level;
    xp : int ref
  }

let ltypes = [Aristocrat; Expert; Warrior]

let make () =
  { ltype = Listx.pick_from ltypes;
    level = Dice.between 3 6;
    xp = ref 0
  }

let lives () =
  Dice.chance 0.95

let won t = incr t.xp
let type_of t = t.ltype
let level_of t = t.level
let can_lvup t = !(t.xp) > 1
let lvup t =
  let xp = !(t.xp) in
  { t with
    level = t.level + (xp / 2);
    xp = ref (xp mod 2)
  }
