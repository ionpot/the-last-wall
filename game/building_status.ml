module B = Building
module R = Resource

type status = Absent | Needs of R.t | Built | Ready
type t =
  { bld : B.t;
    count : int ref;
    mutable status : status
  }

let is_in_need = function
  | Needs _ -> true
  | _ -> false

let next_of = function
  | Needs x ->
      if x = R.empty
      then Built else Needs x
  | Built -> Ready
  | Absent
  | Ready as x -> x

let make_absent bld =
  { bld;
    count = ref 0;
    status = Absent
  }

let make_built bld =
  { bld;
    count = ref 1;
    status = Built
  }

let make bld =
  if B.built bld
  then make_built bld
  else make_absent bld

let cost_of t =
  match t.status with
  | Needs res -> res
  | _ -> R.empty

let which t = t.bld
let count_of t = !(t.count)

let can_start t =
  B.multiple t.bld
  || t.status = Absent

let is b t = t.bld = b
let is_built t = t.status = Built

let is_ready t =
  t.status = Ready
  || count_of t > 0

let start t =
  t.status <- Needs (B.cost_of t.bld)

let tick t =
  let n = next_of t.status in
  t.status <- n;
  if n = Built then incr t.count

let map_res f t =
  let rem, new_n = f (cost_of t) in
  if is_in_need t.status
  then t.status <- Needs new_n;
  rem

let add_manp res t =
  if R.has_supp (cost_of t)
  then res
  else map_res (R.deduce_manp res) t

let add_supp res =
  map_res (R.deduce_supp res)
