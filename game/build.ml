type cost = Resource.t
type kind = Arena | Engrs | Fort | Foundry | Guesthouse | Market | Mausoleum of Leader.t | Observatory | Sawmill | Stable | Tavern | Temple | Trade of Nation.kind option
type bonus = To of kind | ToAll

module Bonus = struct
  type t = bonus * Resource.Bonus.t
  let apply_to = List.fold_left Resource.bonus_to
  let is kind = function
      | (To k, _) -> k = kind
      | (ToAll, _) -> true
  let filter kind = List.filter (is kind)
  let find kind ls = filter kind ls |> List.map snd
end

let trade_default = Trade None
let avlb_default =
  [Arena; Engrs; Fort; Foundry; Market; Sawmill; Stable; Temple; trade_default]
let prebuilt = [Tavern]

let base_cost =
  let open Resource in
  function
  | Arena -> Manpwr 43, Supply 49
  | Engrs -> Manpwr 66, Supply 67
  | Fort -> Manpwr 124, Supply 136
  | Foundry -> Manpwr 28, Supply 30
  | Guesthouse -> Manpwr 21, Supply 23
  | Market -> Manpwr 44, Supply 65
  | Mausoleum _ -> Manpwr 14, Supply 14
  | Observatory -> Manpwr 15, Supply 14
  | Sawmill -> Manpwr 23, Supply 25
  | Stable -> Manpwr 49, Supply 54
  | Tavern -> Manpwr 0, Supply 0
  | Temple -> Manpwr 61, Supply 63
  | Trade _ -> Manpwr 51, Supply 49

let base_cost_of kind =
  let a, b = base_cost kind in
  Resource.(empty <+ a <+ b)

let cost_of kind bonuses =
  Bonus.find kind bonuses
  |> Bonus.apply_to (base_cost_of kind)

let is_multiple kind =
  kind = Stable

let manpwr_range = function
  | Arena -> (2, 8)
  | _ -> (0, 0)

let starting ldr =
  match Leader.kind_of ldr with
  | Leader.Aristocrat -> Stable
  | Leader.Engineer -> Engrs
  | Leader.Merchant -> trade_default

let supply_range = function
  | Foundry -> (9, 15)
  | Market -> (15, 45)
  | Sawmill
  | Tavern -> (6, 12)
  | _ -> (0, 0)

let unlocks = function
  | Temple -> [Guesthouse; Observatory]
  | _ -> []

module Avlb = struct
  module Set = Set.Make(struct type t = kind let compare = compare end)

  type t = Set.t

  let add = Set.add

  let add_ls ls t =
    List.fold_left (Fn.flip add) t ls

  let from = Set.of_list

  let rm kind t =
    if is_multiple kind then t
    else Set.remove kind t

  let rm_ls ls t =
    List.fold_left (Fn.flip rm) t ls

  let unlock kind t =
    add_ls (unlocks kind) t
    |> rm kind

  let unlock_ls ls t =
    List.fold_left (Fn.flip unlock) t ls
end

module Built = struct
  type t = kind list

  let empty : t = []

  let has = List.mem

  let set_trade trade t =
    List.map (fun k -> if k = trade_default then trade else k) t
end

module Queue = struct
  type t = (kind * cost) list

  let empty : t = []

  let built t =
    let f (_, cost) = cost = Resource.empty in
    let built, ongoing = List.partition f t in
    List.map fst built, ongoing

  let from kinds bonuses =
    let f kind = kind, cost_of kind bonuses in
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
  module Map = Map.Make(struct type t = kind let compare = compare end)

  type t = Defs.count Map.t

  let empty : t = Map.empty

  let add kind t =
    Map.add kind 1 t

  let bump kind t =
    let f = function
      | Some x -> Some (succ x)
      | None -> Some 1
    in
    Map.update kind f t

  let bump_ls ls t =
    List.fold_left (Fn.flip bump) t ls

  let count kind t =
    if Map.mem kind t
    then Map.find kind t
    else 0

  let decr kind t =
    let f = function
      | Some x -> Number.sub_opt x 1
      | None -> None
    in
    Map.update kind f t

  let from ls =
    bump_ls ls empty

  let has = Map.mem

  let set_trade trade t =
    Map.remove trade_default t
    |> add trade

  let sum t =
    Map.fold (fun _ -> (+)) t 0

  let mausoleums t =
    Map.filter (fun k _ -> match k with Mausoleum _ -> true | _ -> false) t
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
  ; ready = Ready.from prebuilt
  }

let available t = t.avlb

let count kind t =
  Ready.count kind t.ready

let arena_cap t =
  count Arena t * 10

let has_trade nation t =
  Ready.has (Trade (Some nation)) t.ready

let is_built kind t =
  Built.has kind t.built

let is_ready kind t =
  Ready.has kind t.ready

let ballista_cap t =
  if is_ready Engrs t then 5 else 0

let mausoleums t =
  Ready.mausoleums t.ready

let need_manp t =
  Queue.sum_fn Resource.manp_of t.queue

let need_supp t =
  Queue.sum_fn Resource.supp_of t.queue

let need_trade t =
  Built.has trade_default t.built
  || Ready.has trade_default t.ready

let ready t = t.ready

let stable_cap t =
  count Stable t * 10

let status t =
  let built, ongoing = Queue.built t.queue in
  t.built, built, ongoing

let temple_cap t =
  if is_ready Guesthouse t then 40
  else if is_ready Temple t then 20
  else 0

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

let set_trade nation_opt t =
  let trade = Trade nation_opt in
  { t with built = Built.set_trade trade t.built
  ; ready = Ready.set_trade trade t.ready
  }

let start kinds bonuses t =
  { t with avlb = Avlb.rm_ls kinds t.avlb
  ; queue = Queue.from kinds bonuses @ t.queue
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
