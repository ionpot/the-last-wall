type kind = Arena | Barracks | Engrs | Fort | Foundry | Guesthouse | Market | Mausoleum of Leader.t | Observatory | Sawmill | Stable | Tavern | Temple | Trade

module Map = Map.Make(struct type t = kind let compare = compare end)
module Mapx = Mapx.Make(Map)

type cost = Resource.t
type cost_map = cost Map.t

module Bonus = struct
  type target = To of kind | ToAll
  type bonus = Resource.Bonus.t
  type t = target * bonus
  let is kind = function To k -> k = kind | ToAll -> true
  let to_cost (target, bonus) =
    let f k res =
      if is k target then Resource.bonus_to res bonus else res
    in Map.mapi f
  let to_cost_if cond t map =
    if cond then to_cost t map else map
end

let avlb_default =
  [Arena; Barracks; Engrs; Fort; Foundry; Market; Sawmill; Stable; Temple; Trade]

let base_cap = function
  | Arena -> 10
  | Engrs -> 5
  | Guesthouse -> 20
  | Stable -> 10
  | Temple -> 20
  | _ -> 0

let base_cost =
  let open Resource in
  function
  | Arena -> Manpwr 43, Supply 49
  | Barracks -> Manpwr 65, Supply 70
  | Engrs -> Manpwr 60, Supply 62
  | Fort -> Manpwr 124, Supply 136
  | Foundry -> Manpwr 28, Supply 30
  | Guesthouse -> Manpwr 21, Supply 23
  | Market -> Manpwr 44, Supply 65
  | Mausoleum _ -> Manpwr 14, Supply 14
  | Observatory -> Manpwr 15, Supply 14
  | Sawmill -> Manpwr 23, Supply 25
  | Stable -> Manpwr 49, Supply 54
  | Tavern -> Manpwr 39, Supply 41
  | Temple -> Manpwr 61, Supply 63
  | Trade -> Manpwr 51, Supply 49

let base_cost_of kind =
  let a, b = base_cost kind in
  Resource.(empty <+ a <+ b)

let is_multiple kind =
  kind = Stable

let manpwr_range = function
  | Arena -> (2, 8)
  | _ -> (0, 0)

let supply_range = function
  | Foundry -> (9, 15)
  | Market -> (15, 45)
  | Sawmill
  | Tavern -> (6, 12)
  | _ -> (0, 0)

let unlocks = function
  | Temple -> [Guesthouse; Observatory]
  | _ -> []

let fn_ls f ls t =
  List.fold_left (Fn.flip f) t ls

module Avlb = struct
  module Set = Set.Make(struct type t = kind let compare = compare end)

  type t = Set.t

  let add = Set.add

  let add_ls = fn_ls add

  let from = Set.of_list

  let rm kind t =
    if is_multiple kind then t
    else Set.remove kind t

  let rm_ls = fn_ls rm

  let unlock kind t =
    add_ls (unlocks kind) t
    |> rm kind

  let unlock_ls = fn_ls unlock
end

module Built = struct
  type t = kind list

  let empty : t = []

  let has = List.mem
end

module Queue = struct
  type t = (kind * cost) list

  let empty : t = []

  let built t =
    let f (_, cost) = cost = Resource.empty in
    let built, ongoing = List.partition f t in
    List.map fst built, ongoing

  let cost_of kind costs =
    if Map.mem kind costs
    then Map.find kind costs
    else base_cost_of kind

  let from kinds costs =
    let f kind = kind, cost_of kind costs in
    List.rev_map f kinds

  let map f need avlb t =
    let f' acc (kind, cost) =
      let acc', cost' = f acc cost in
      acc', (kind, cost')
    in
    let x = Number.sub need avlb in
    Listx.map_with f' x t

  let sum_fn fn t =
    let f acc (_, res) = acc + fn res in
    List.fold_left f 0 t
end

module Ready = struct
  type t = Defs.count Map.t

  let empty : t = Map.empty

  let bump = Mapx.Int.succ
  let bump_ls = fn_ls bump
  let count = Mapx.Int.value
  let decr = Mapx.Int.pred
  let has = Map.mem
  let sum = Mapx.Int.sum

  let mausoleums t =
    Mapx.filterk (function Mausoleum _ -> true | _ -> false) t
    |> sum
end

type status = Built.t * Built.t * Queue.t

type t =
  { avlb : Avlb.t
  ; built : Built.t
  ; queue : Queue.t
  ; ready : Ready.t
  }

let empty =
  { avlb = Avlb.from avlb_default
  ; built = Built.empty
  ; queue = Queue.empty
  ; ready = Ready.empty
  }

let available t = t.avlb

let cost_map t =
  let f kind = Map.add kind (base_cost_of kind) in
  Avlb.Set.fold f t.avlb (Map.empty : cost_map)

let count kind t =
  Ready.count kind t.ready

let cap_of kind t =
  count kind t * base_cap kind

let is_built kind t =
  Built.has kind t.built

let is_ready kind t =
  Ready.has kind t.ready

let is_complete kind t =
  is_built kind t || is_ready kind t

let built t = t.built

let mausoleums t =
  Ready.mausoleums t.ready

let need_manp t =
  Queue.sum_fn Resource.manp_of t.queue

let need_supp t =
  Queue.sum_fn Resource.supp_of t.queue

let queue t = t.queue

let ready t = t.ready

let status t =
  let built, ongoing = Queue.built t.queue in
  t.built, built, ongoing

let died ldr t =
  { t with avlb = Avlb.add (Mausoleum ldr) t.avlb }

let manp need avlb t =
  let f mp cost =
    if Resource.has_supp cost
    then mp, cost
    else Resource.take_manp mp cost
  in
  { t with queue = Queue.map f need avlb t.queue }

let raze kind t =
  { t with avlb = Avlb.add kind t.avlb
  ; ready = Ready.decr kind t.ready
  }

let set_ready kind t =
  { t with avlb = Avlb.unlock kind t.avlb
  ; ready = Ready.bump kind t.ready
  }

let set_ready_ls = fn_ls set_ready

let start kinds costs t =
  { t with avlb = Avlb.rm_ls kinds t.avlb
  ; queue = Queue.from kinds costs @ t.queue
  }

let supp need avlb t =
  let f = Resource.take_supp in
  { t with queue = Queue.map f need avlb t.queue }

let update (ready, built, queue) t =
  { avlb = Avlb.unlock_ls built t.avlb
  ; built
  ; queue
  ; ready = Ready.bump_ls ready t.ready
  }
