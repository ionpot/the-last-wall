open Defs

module Bonus = struct
  type kind = Mnp of float | Sup of float | Both of float
  type t = Add of kind | Sub of kind
end

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
let set_manp (_, x) y = (y, x)
let set_supp (x, _) y = (x, y)
let mis_supp (_, x) = of_supp ~-(min 0 x)
let clr_supp (x, _) = (x, 0)
let manp2supp (x, _) = (0, x)
let supp2manp (_, x) = (x, 0)

let deduce_manp m res =
  let m2, m3 = Number.deduce m (manp_of res) in
  m2, set_manp res m3

let deduce_supp s res =
  let s2, s3 = Number.deduce s (supp_of res) in
  s2, set_supp res s3

let take_manp m res =
  let m2, m3 = Number.take m (manp_of res) in
  m2, set_manp res m3

let take_supp s res =
  let s2, s3 = Number.take s (supp_of res) in
  s2, set_supp res s3

let map_manp f res =
  set_manp res (f (manp_of res))

let map_supp f res =
  set_supp res (f (supp_of res))

let bonus_to t =
  let rec apply f kind t =
    match kind with
    | Bonus.Both ratio ->
        apply f (Bonus.Mnp ratio) t
        |> apply f (Bonus.Sup ratio)
    | Bonus.Mnp ratio ->
        map_manp (f ratio) t
    | Bonus.Sup ratio ->
        map_supp (f ratio) t
  in
  function
  | Bonus.Add x -> apply Number.increase_by x t
  | Bonus.Sub x -> apply Number.reduce_by x t

let bonus_if cond bonus t =
  if cond then bonus_to t bonus else t

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
