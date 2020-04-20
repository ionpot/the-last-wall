type kind = Comet | Disease | Tavern

let kinds = [Comet; Disease; Tavern]

type t = kind option

let empty : t = None

let has kind = function
  | Some k -> k = kind
  | None -> false

module Roll = struct
  let from check =
    let ok = Dice.chance 0.05 in
    (if ok then List.filter check kinds else [])
    |> function
        | [] -> None
        | ls -> Some (Dice.pick ls)
end
