type ltype = Aristocrat | Expert | Warrior
type level = int

type t =
  { ltype : ltype;
    level : level;
    wins : int ref
  }

let ltypes = [Aristocrat; Expert; Warrior]

let make () =
  { ltype = Listx.pick_from ltypes;
    level = Dice.between 3 6;
    wins = ref 0
  }

let lives () =
  Dice.chance 0.95

let won t = incr t.wins
let type_of t = t.ltype
let level_of t = t.level + !(t.wins) / 2
