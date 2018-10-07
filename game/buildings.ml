module B = Building
module S = Building_status

type queued = (B.t * Resource.t)
type t =
  { stats : S.t list;
    queue : S.t Queue.t
  }

let make () =
  { stats = List.map S.make B.tlist;
    queue = Queue.create ()
  }

let s_of t b =
  List.find (S.is b) t.stats

let count_of b t =
  S.count_of (s_of t b)

let is_ready b t =
  S.is_ready (s_of t b)

let start t s =
  Queue.add s t.queue;
  S.start s

let build ls t =
  ls |> List.map (s_of t)
  |> List.filter S.can_start
  |> List.iter (start t)

let draw f res t =
  Queue.fold f res t.queue

let draw_manp = draw S.add_manp
let draw_supp = draw S.add_supp

let in_queue t =
  Queuex.map_to_list (fun s -> S.(which s, cost_of s)) t.queue

let take_if_built q =
  if (Queue.is_empty q)
  then None
  else
    if S.is_built (Queue.peek q)
    then Some (Queue.take q)
    else None

let rec take_built q ls =
  match take_if_built q with
  | Some s -> take_built q (s :: ls)
  | None -> ls

let tick t =
  List.iter S.tick t.stats;
  take_built t.queue []
  |> List.map S.which
