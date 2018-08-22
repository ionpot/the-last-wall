module R = Resource

type mercs = R.t

let chance x =
  if Dice.chance 0.8
  then Some (R.of_manp x)
  else None

let roll () =
  let x = Dice.between 10 30 in
  chance x

let buy mercs res =
  let total_cost = R.manp2supp mercs in
  let remaining = R.(res -- total_cost) in
  let mis_sp = R.mis_supp remaining in
  let mis_mp = R.supp2manp mis_sp in
  R.(remaining ++ mercs ++ mis_sp -- mis_mp)
