module R = Resource
module S = Building_status

type t =
  { status : S.t;
    mutable needs : R.t
  }

let make s =
  { status = s;
    needs = S.cost_of s
  }

let is_built t = t.needs = R.empty

let start t = S.start t.status
let fin t = S.fin t.status

let map_res f t =
  let rem, new_n = f t.needs in
  t.needs <- new_n;
  rem

let add_manp res t =
  if R.has_supp t.needs
  then res
  else map_res (R.deduce_manp res) t

let add_supp res =
  map_res (R.deduce_supp res)
