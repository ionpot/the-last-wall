type cost = Resource.t
type kind = Engrs | Fort | Market | Mausoleum of Leader.t | Observatory | Stable | Tavern | Temple | Trade of Nation.trade
type bonus = To of kind | ToAll
type queued = kind * cost
type status = kind list * kind list * queued list

module Bonus = struct
  type t = bonus * Resource.Bonus.t
  let apply_to = List.fold_left Resource.bonus_to
  let is kind = function
      | (To k, _) -> k = kind
      | (ToAll, _) -> true
  let filter kind = List.filter (is kind)
  let find kind ls = filter kind ls |> List.map snd
end

type t =
  { avlb : kind list;
    built : kind list;
    queue : queued list;
    ready : kind list
  }

let enables built avlb =
  if List.mem Temple built
  then Observatory :: avlb
  else avlb

let multiple kind =
  kind = Stable

let rm_ls kinds ls =
  Listx.discard multiple kinds
  |> Listx.rm_from ls

let empty =
  { avlb = [Engrs; Fort; Market; Stable; Temple; Trade Nation.NoTrade];
    built = [];
    queue = [];
    ready = [Tavern]
  }

let base_cost =
  let open Resource in
  function
  | Engrs -> Manpwr 66, Supply 67
  | Fort -> Manpwr 124, Supply 136
  | Market -> Manpwr 44, Supply 65
  | Mausoleum _ -> Manpwr 14, Supply 14
  | Observatory -> Manpwr 15, Supply 14
  | Stable -> Manpwr 49, Supply 54
  | Tavern -> Manpwr 0, Supply 0
  | Temple -> Manpwr 54, Supply 56
  | Trade _ -> Manpwr 51, Supply 49

let base_cost_of kind =
  let a, b = base_cost kind in
  Resource.(empty <+ a <+ b)

let built kind t =
  List.mem kind t.built

let count kind t =
  Listx.count kind t.ready

let ls_avlb t = t.avlb
let ls_built t = t.built
let ls_queue t = t.queue
let ls_ready t = t.ready

let mausoleums t =
  t.ready
  |> List.filter (function Mausoleum _ -> true | _ -> false)
  |> List.length

let need_manp t =
  let f acc (_, res) = acc + Resource.manp_of res in
  List.fold_left f 0 t.queue

let need_supp t =
  let f acc (_, res) = acc + Resource.supp_of res in
  List.fold_left f 0 t.queue

let ready kind t =
  List.mem kind t.ready

let cost_of kind bonuses =
  Bonus.find kind bonuses
  |> Bonus.apply_to (base_cost_of kind)

let status t =
  let f (_, cost) = cost = Resource.empty in
  let built, ongoing = List.partition f t.queue in
  t.built, List.map fst built, ongoing

let trade t =
  let f x = function Trade x -> x | _ -> x in
  List.fold_left f Nation.NoTrade t.ready

let map_queue f need avlb t =
  let f' acc (kind, cost) =
    let acc', cost' = f acc cost in
    acc', (kind, cost')
  in
  let x = Number.sub need avlb in
  let queue = Listx.map_with f' x t.queue in
  { t with queue }

let manp need avlb t =
  let f mp cost =
    if Resource.has_supp cost
    then mp, cost
    else Resource.take_manp mp cost
  in
  map_queue f need avlb t

let to_avlb kind t =
  let ls = t.avlb in
  let avlb =
    if multiple kind then ls
    else kind :: ls
  in
  { t with avlb }

let died ldr t =
  to_avlb (Mausoleum ldr) t

let enqueue kinds bonuses t =
  let f kind = kind, cost_of kind bonuses in
  let ls = List.rev_map f kinds in
  { t with queue = ls @ t.queue }

let raze kind t =
  { t with ready = Listx.rm kind t.ready }
  |> to_avlb kind

let set_trade trade t =
  let f = List.map (function Trade _ -> Trade trade | x -> x) in
  { t with built = f t.built; ready = f t.ready }

let start kinds bonuses t =
  { t with avlb = rm_ls kinds t.avlb }
  |> enqueue kinds bonuses

let supp need avlb t =
  map_queue Resource.take_supp need avlb t

let update (ready, built, queue) t =
  { avlb = enables built t.avlb;
    built;
    queue;
    ready = ready @ t.ready
  }
