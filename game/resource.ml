type manpower = int
type supply = int

type t = (manpower * supply)

type kind =
  | Empty
  | Manpwr of manpower
  | Supply of supply

let empty = (0, 0)
let of_manp x = (x, 0)
let of_supp x = (0, x)
let manp_of = fst
let supp_of = snd
let has_manp (x, _) = x > 0
let has_supp (_, x) = x > 0
let mis_supp (_, x) =
  of_supp (if x < 0 then abs x else 0)
let clr_supp (x, _) = (x, 0)
let cost_of (m, _) = (0, m)
let can_afford (m, _) (_, s) = m < s

let (<+) t = function
  | Empty -> t
  | Manpwr x -> Pair.(+<) x t
  | Supply x -> Pair.(+>) x t

let (<~) t = function
  | Empty -> t
  | Manpwr x -> Pair.(~<) x t
  | Supply x -> Pair.(~>) x t

let (++) = Pair.(++)
let (--) = Pair.(--)
