module Bonus = struct
  type kind = Mnp of float | Sup of float | Both of float
  type t = Add of kind | Sub of kind
end

type t =
  { mnp : Defs.manpower
  ; sup : Defs.supply
  }

let empty = { mnp = 0; sup = 0 }

let make ?(mnp=0) ?(sup=0) () =
  { mnp; sup }

let has res t =
  res.mnp <= t.mnp
  && res.sup <= t.sup

let has_mnp t = t.mnp > 0
let has_sup t = t.sup > 0

let mnp t = t.mnp
let sup t = t.sup

let (++) a b =
  { mnp = a.mnp + b.mnp
  ; sup = a.sup + b.sup
  }

let (--) a b =
  { mnp = a.mnp - b.mnp
  ; sup = a.sup - b.sup
  }

let add ?mnp ?sup t =
  t ++ make ?mnp ?sup ()

let map_mnp f t =
  { t with mnp = f t.mnp }

let map_sup f t =
  { t with sup = f t.sup }

let bonus bonus t =
  let rec apply f kind t =
    match kind with
    | Bonus.Both ratio ->
        apply f (Bonus.Mnp ratio) t
        |> apply f (Bonus.Sup ratio)
    | Bonus.Mnp ratio ->
        map_mnp (f ratio) t
    | Bonus.Sup ratio ->
        map_sup (f ratio) t
  in
  match bonus with
  | Bonus.Add x -> apply Number.increase_by x t
  | Bonus.Sub x -> apply Number.reduce_by x t

let bonus_if cond b t =
  if cond then bonus b t else t

let deduce a b =
  let ma, mb = Number.deduce a.mnp b.mnp in
  let sa, sb = Number.deduce a.sup b.sup in
  make ~mnp:ma ~sup:sa (),
  make ~mnp:mb ~sup:sb ()
