module B = Building
module C = Construction
module S = Building_status

type t =
  { stats : S.t list;
    built : C.t Stack.t;
    queue : C.t Queue.t
  }

let make () =
  { stats = List.map S.make B.tlist;
    built = Stack.create ();
    queue = Queue.create ()
  }

let s_of t b =
  t.stats
  |> List.filter (S.is b)
  |> List.hd

let count_of b t =
  S.count_of (s_of t b)

let is_ready b t =
  S.is_ready (s_of t b)

let start t cons =
  Queue.add cons t.queue;
  C.start cons

let build ls t =
  ls |> List.map (s_of t)
  |> List.filter S.can_start
  |> List.map C.make
  |> List.iter (start t)

let draw f res t =
  Queue.fold (fun r c -> f r c) res t.queue

let draw_manp = draw C.add_manp
let draw_supp = draw C.add_supp

let take_if cond f q =
  if not (Queue.is_empty q)
  then
    if cond (Queue.peek q)
    then f (Queue.take q)

let rec to_built t =
  take_if C.is_built (fun c ->
    Stack.push c t.built;
    to_built t) t.queue

let fin_built t =
  Stack.iter C.fin t.built;
  Stack.clear t.built

let tick t =
  fin_built t;
  to_built t
