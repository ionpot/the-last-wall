module B = Building
module Q = Building_queue

type queued = (B.t * Resource.t)
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

let apply_manp m t =
  { t with queue = Q.apply_manp m t.queue }

let deduce supp t =
  let remaining, queue = Q.deduce supp t.queue in
  remaining, { t with queue }

let built t = t.built

let in_queue t =
  Q.status_of t.queue

let tick t =
  let built, queue = Q.tick t.queue in
  { t with
    built;
    queue;
    ready = t.ready @ t.built
  }
