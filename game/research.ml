type kind = BlackArmy

module Cost = struct
  let men = function
    | BlackArmy -> 13

  let supply = function
    | BlackArmy -> 18

  let resource k =
    Resource.(of_manp (men k) <+ Supply (supply k))
end

module Kind = struct
  type t = kind
  let compare = compare
end

module Map = Map.Make(Kind)
module Mapx = Mapx.Make(Map)
module Set = Set.Make(Kind)
module Queue = Queue.Make(Set)

let map2set m =
  Map.fold (fun k _ s -> Set.add k s) m Set.empty

module type Progress = sig
  val cancelled : Set.t
  val men_used : Defs.manpower
  val rem_supply : Defs.supply
  val started : Set.t
end

module Status = struct
  type value =
    | Available
    | Complete
    | Progress of Defs.turn

  type t = value Map.t

  let empty : t = Map.empty

  let available t = Mapx.filterv ((=) Available) t |> map2set
  let complete t = Mapx.filterv ((=) (Progress 0)) t |> map2set
  let is_complete kind t = Mapx.maybe ((=) Complete) false kind t

  let set_available kind t =
    if Map.mem kind t then t
    else Map.add kind Available t
  let set_available_set = Set.fold (fun k t -> set_available k t)
  let set_complete = Set.fold (fun k t -> Map.add k Complete t)
  let start = Set.fold (fun k t -> Map.add k (Progress 1) t)
  let tick t =
    let f = function
      | Progress x -> Progress (Number.sub x 1)
      | x -> x
    in Map.map f t
end

type t =
  { queue : Queue.t
  ; status : Status.t
  }

let empty =
  { queue = Queue.empty
  ; status = Status.empty
  }

let apply men supply t =
  let res = Resource.(of_supp supply <+ Manpwr men) in
  let rem, pass, fail = Queue.partition res t.queue in
  let used = Queue.cost pass in
  (module struct
    let cancelled = Queue.to_set fail
    let men_used = Resource.manp_of used
    let rem_supply = Resource.supp_of rem
    let started = Queue.to_set pass
  end : Progress)

let available t =
  Status.available t.status

let complete t =
  Status.complete t.status

let is_complete kind t =
  Status.is_complete kind t.status

let set_available kind t =
  { t with status = Status.set_available kind t.status }

let set_complete set t =
  { t with status = Status.set_complete set t.status }

let set_progress (module S : Progress) t =
  { queue = Queue.empty
  ; status = t.status
      |> Status.set_available_set S.cancelled
      |> Status.start S.started
  }

let start ls t =
  let f q k = Queue.add k (Cost.resource k) q in
  { t with queue = List.fold_left f t.queue ls }

let tick t =
  { t with status = Status.tick t.status }

let unlock kind yes t =
  if yes then set_available kind t else t
