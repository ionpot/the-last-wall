type kind = Arena | Barracks | Engrs | Fort | Foundry | Guesthouse | Market | Mausoleum of Leader.t | Observatory | Sawmill | Stable | Tavern | Temple | Trade | Workshop

module Kind = struct
  type t = kind
  let compare = compare
end
module Map = Map.Make(Kind)
module Mapx = Mapx.Make(Map)
module Set = Set.Make(Kind)
module Queue = Queue.Make(Set)

type cost = Resource.t
type cost_map = cost Map.t

let avlb_default =
  [Arena; Barracks; Engrs; Fort; Foundry; Market; Sawmill; Stable; Temple; Trade]

let base_cap = function
  | Arena -> 10
  | Engrs -> 5
  | Guesthouse -> 20
  | Stable -> 10
  | Temple -> 20
  | Workshop -> 2
  | _ -> 0

let base_cost =
  let open Resource in
  function
  | Arena -> make ~mnp:43 ~sup:49 ()
  | Barracks -> make ~mnp:65 ~sup:70 ()
  | Engrs -> make ~mnp:57 ~sup:59 ()
  | Fort -> make ~mnp:124 ~sup:136 ()
  | Foundry -> make ~mnp:28 ~sup:30 ()
  | Guesthouse -> make ~mnp:21 ~sup:23 ()
  | Market -> make ~mnp:44 ~sup:65 ()
  | Mausoleum _ -> make ~mnp:14 ~sup:14 ()
  | Observatory -> make ~mnp:15 ~sup:14 ()
  | Sawmill -> make ~mnp:23 ~sup:25 ()
  | Stable -> make ~mnp:49 ~sup:54 ()
  | Tavern -> make ~mnp:39 ~sup:41 ()
  | Temple -> make ~mnp:61 ~sup:63 ()
  | Trade -> make ~mnp:51 ~sup:49 ()
  | Workshop -> make ~mnp:13 ~sup:14 ()

let is_multiple kind =
  kind = Stable

let manpwr_range = function
  | Arena -> (3, 12)
  | _ -> (0, 0)

let supply_range = function
  | Foundry -> (9, 15)
  | Market -> (15, 45)
  | Sawmill
  | Tavern -> (6, 12)
  | _ -> (0, 0)

let unlocks = function
  | Engrs -> [Workshop]
  | Temple -> [Guesthouse; Observatory]
  | _ -> []

let fn_ls f ls t =
  List.fold_left (Fn.flip f) t ls

module Avlb = struct
  type t = Set.t

  let add = Set.add

  let add_ls = fn_ls add

  let from = Set.of_list

  let rm kind t =
    if is_multiple kind then t
    else Set.remove kind t

  let to_map t =
    let f k map = Map.add k (base_cost k) map in
    Set.fold f t Map.empty

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

let count kind t =
  Ready.count kind t.ready

let cap_of kind t =
  count kind t * base_cap kind

let cost_map t =
  Avlb.to_map t.avlb

let is_built kind t =
  Built.has kind t.built

let is_ready kind t =
  Ready.has kind t.ready

let is_complete kind t =
  is_built kind t || is_ready kind t

let built t = t.built

let mausoleums t =
  Ready.mausoleums t.ready

let needs t = Queue.cost t.queue

let queue t = t.queue

let ready t = t.ready

let status t =
  let built, ongoing = Queue.pop_finished t.queue in
  t.built, built, ongoing

let apply_mnp mnp t =
  let res = Resource.make ~mnp () in
  let cond cost = Resource.has_sup cost |> not in
  { t with queue = Queue.apply_if cond res t.queue |> snd }

let apply_sup sup t =
  let res = Resource.make ~sup () in
  let rem, queue = Queue.apply res t.queue in
  Resource.sup rem, { t with queue }

let died ldr t =
  { t with avlb = Avlb.add (Mausoleum ldr) t.avlb }

let raze kind t =
  { t with avlb = Avlb.add kind t.avlb
  ; ready = Ready.decr kind t.ready
  }

let set_ready kind t =
  { t with avlb = Avlb.unlock kind t.avlb
  ; ready = Ready.bump kind t.ready
  }

let set_ready_ls = fn_ls set_ready

let start kind cost_map t =
  let cost = Map.find kind cost_map in
  { t with avlb = Avlb.rm kind t.avlb
  ; queue = Queue.add kind cost t.queue
  }

let update (ready, built, queue) t =
  { avlb = Avlb.unlock_ls built t.avlb
  ; built
  ; queue
  ; ready = Ready.bump_ls ready t.ready
  }
