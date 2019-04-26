type name = { first : string; house : string; title : string }
type t = { chosen : name; prev : name list }

let empty_name = { first = ""; house = ""; title = "" }
let empty = { chosen = empty_name; prev = [] }

let first t = t.chosen.first
let house t = t.chosen.house
let title t = t.chosen.title

module Roll (Dice : Dice.S) = struct
  let new_prev t =
    if t.chosen = empty_name then t.prev else t.chosen :: t.prev

  let pick = function
    | [] -> ""
    | ls -> Dice.pick ls

  let make first house title =
    { first = pick first; house = pick house; title = pick title }

  let from first house title t =
    let rec choose () =
      let name = make first house title in
      if List.mem name t.prev then choose () else name
    in
    { chosen = choose (); prev = new_prev t }
end
