type t =
  { bld : Building.t;
    count : int ref;
    mutable started : bool
  }

let starting_count b =
  if Building.built b then 1 else 0

let make bld =
  { bld;
    count = ref (starting_count bld);
    started = false
  }

let cost_of t =
  Building.cost_of t.bld

let count_of t = !(t.count)
let is_ready t = count_of t > 0
let is b t = t.bld = b
let can_start t =
  if Building.multiple t.bld
  then true
  else not t.started

let start t = t.started <- true
let fin t =
  t.started <- false;
  incr t.count
