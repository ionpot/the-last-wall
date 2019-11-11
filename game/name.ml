type name = { first : string; house : string }
type t =
  { name : name
  ; prev : name list
  ; title : string
  }

let empty_name = { first = ""; house = "" }
let empty =
  { name = empty_name
  ; prev = []
  ; title = ""
  }

let first t = t.name.first
let house t = t.name.house
let title t = t.title

let full t =
  [first t; house t; title t]
  |> List.filter ((<>) "")
  |> String.concat " "

module Roll (Dice : Dice.S) = struct
  let new_prev t =
    if t.name = empty_name then t.prev
    else t.name :: t.prev

  let pick = function
    | [] -> ""
    | ls -> Dice.pick ls

  let make first house =
    { first = pick first; house = pick house }

  let from first house title t =
    let rec choose () =
      let name = make first house in
      if List.mem name t.prev then choose () else name
    in
    { name = choose ()
    ; prev = new_prev t
    ; title = pick title
    }
end
