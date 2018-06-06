open Defs

type t = (manpower * supply)

type kind =
  | Empty
  | Manpwr of manpower
  | Supply of supply

let empty = 0, 0

let make = function
  | Empty -> empty
  | Manpwr x -> x, 0
  | Supply x -> 0, x

let manp t = fst t
let supp t = snd t
let supp_missing (_, s) =
  if s < 0 then abs s else 0

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
