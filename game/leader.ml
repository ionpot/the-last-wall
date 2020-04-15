type charisma = Defs.count
type gender = Female | Male
type kind = Aristocrat | Engineer | Merchant
type level = Defs.count

type t =
  { cha : charisma
  ; died : Defs.turn
  ; gender : gender
  ; kind : kind
  ; level : level
  ; name : Name.t
  ; noble : bool
  ; respawn : Defs.count
  ; xp : Defs.count
  }

let empty =
  { cha = 0
  ; died = 0
  ; gender = Female
  ; kind = Aristocrat
  ; level = 0
  ; name = Name.empty
  ; noble = true
  ; respawn = 0
  ; xp = 0
  }

let kinds = [Aristocrat; Engineer; Merchant]

let cha_of t = t.cha
let cha_mod_of t = (t.cha - 10) / 2
let defense_of t = 0.1 +. (0.01 *. float t.level)
let gender_of t = t.gender
let is kind t = t.kind = kind
let is_alive t = t.died = 0
let is_dead t = t.died > 0
let is_living kind t = is_alive t && is kind t
let is_noble t = t.noble
let can_level_up t = is_alive t && t.xp mod 2 = 0
let can_respawn turn t =
  is_dead t && t.died + t.respawn <= turn
let kind_of t = t.kind
let level_of t = t.level
let name_of t = t.name
let victories t = t.xp

let add_cha t =
  let i =
    if t.level mod 5 = 0 then
      if t.cha < 18 then 2 else 1
    else 0
  in
  { t with cha = t.cha + i }

let died respawn died t =
  { t with died; respawn }

let level_up t =
  { t with level = t.level + 1 }
  |> add_cha

let won t =
  { t with xp = t.xp + 1 }

module Roll = struct
  let death t =
    if is_alive t then Dice.chance 0.05 else false

  let name t =
    let first = Names.(if t.gender = Female then female else male) in
    let house = if t.noble then Names.house else [] in
    { t with name = Name.Roll.from first house Names.title t.name }

  let noble = function
    | Aristocrat -> true
    | Engineer -> Dice.chance 0.2
    | Merchant -> Dice.chance 0.4

  let from kind =
    { empty with cha = Dice.between 10 15
    ; gender = if Dice.yes () then Male else Female
    ; kind
    ; level = Dice.between 3 5
    ; noble = noble kind
    } |> add_cha |> name

  let pair () =
    let a, kinds' = Dice.pop kinds in
    let b = Dice.pick kinds' in
    from a, from b
end
