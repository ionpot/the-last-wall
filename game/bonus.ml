type kind = Barrage | Clan | Training

module Set = Set.Make(struct
  type t = kind
  let compare = compare
end)

type t = Set.t

let empty : t = Set.empty

let has = Set.mem

let set kind cond t =
  (if cond then Set.add else Set.remove) kind t
