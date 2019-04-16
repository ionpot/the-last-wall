type cost = Resource.t
type kind = Fort | Market | Stable | Tavern | Temple
type queued = kind * cost
type status = kind list * kind list * queued list
type t =
  { avlb : kind list;
    built : kind list;
    queue : queued list;
    ready : kind list
  }

let multiple kind =
  kind = Stable

let to_avlb kind ls =
  if multiple kind then ls
  else kind :: ls

let rm_ls kinds ls =
  Listx.discard multiple kinds
  |> Listx.rm_from ls

let empty =
  { avlb = [Fort; Market; Stable; Temple];
    built = [];
    queue = [];
    ready = [Tavern]
  }

let cost_pair_of =
  let open Resource in
  function
  | Fort -> Manpwr 124, Supply 136
  | Market -> Manpwr 44, Supply 65
  | Stable -> Manpwr 49, Supply 54
  | Tavern -> Manpwr 0, Supply 0
  | Temple -> Manpwr 29, Supply 28

let cost_of kind =
  let a, b = cost_pair_of kind in
  Resource.(empty <+ a <+ b)

let available t = t.avlb

let count kind t =
  Listx.count kind t.ready

let need_manp t =
  let f acc (_, res) = acc + Resource.manp_of res in
  List.fold_left f 0 t.queue

let need_supp t =
  let f acc (_, res) = acc + Resource.supp_of res in
  List.fold_left f 0 t.queue

let ready kind t =
  List.mem kind t.ready

let present kind t =
  ready kind t
  || List.mem kind t.built
  || List.exists (fun (k, _) -> k = kind) t.queue

let status t =
  let f (_, cost) = cost = Resource.empty in
  let built, ongoing = List.partition f t.queue in
  t.built, List.map fst built, ongoing

let map_queue f x t =
  let f' acc (kind, cost) =
    let acc', cost' = f acc cost in
    acc', (kind, cost')
  in
  let queue = Listx.map_with f' x t.queue in
  { t with queue }

let manp m t =
  let f mp cost =
    if Resource.has_supp cost
    then mp, cost
    else Resource.take_manp mp cost
  in
  map_queue f m t

let raze kind t =
  { t with
    avlb = to_avlb kind t.avlb;
    ready = Listx.rm kind t.ready }

let start kinds t =
  let ls = Listx.in_both kinds t.avlb in
  let f kind = kind, cost_of kind in
  { t with avlb = rm_ls ls t.avlb;
    queue = List.rev_map f ls @ t.queue }

let supp s t =
  map_queue Resource.take_supp s t

let update (ready, built, queue) t =
  { t with built; queue; ready = ready @ t.ready }
