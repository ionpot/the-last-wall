module B = Building
module Q = Building_queue

type queued = (B.t * Resource.t)
type status = B.t list * B.t list * queued list
type t =
  { built : B.t list;
    ignored : B.t list;
    queue : Q.t;
    ready : B.t list
  }

let empty =
  { built = [];
    ignored = Listx.discard B.multiple B.ready;
    queue = Q.empty;
    ready = B.ready
  }

let count_of b t =
  Listx.count b t.ready

let is_ignored b t =
  List.mem b t.ignored

let is_ready b t =
  List.mem b t.ready

let ignore b bs =
  if B.multiple b || List.mem b bs
  then bs
  else b :: bs

let start t b =
  let ignored = ignore b t.ignored in
  let queue = Q.add b t.queue in
  { t with ignored; queue }

let can_start t b =
  B.multiple b || not (List.mem b t.ignored)

let build ls t =
  List.filter (can_start t) ls
  |> List.fold_left start t

let raze b t =
  let rm_from = Listx.rm b in
  let ready = rm_from t.ready in
  let ignored = rm_from t.ignored in
  { t with ignored; ready }

let manp_cost t = Q.manp_cost t.queue
let supp_cost t = Q.supp_cost t.queue

let apply_manp m need t =
  let mis = Number.sub need m in
  { t with queue = Q.set_manp mis t.queue }

let deduce supp need t =
  let rem, mis = Number.deduce supp need in
  let queue = Q.set_supp mis t.queue in
  rem, { t with queue }

let to_status t =
  let built, ongoing = Q.status_of t.queue in
  t.built, built, ongoing

let update (ready, built, ongoing) t =
  { t with
    built;
    queue = Q.update ongoing t.queue;
    ready = t.ready @ ready
  }
