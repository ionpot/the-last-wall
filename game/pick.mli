open Defs

module type Num = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module Int : Num with type t = int
module Float : Num with type t = float

module type Ops = sig
  module Num : Num
  type key
  type pair = key * Num.t
  val choose : pair list -> pair
  val roll : pair -> Num.t
  val trim : Num.t -> pair -> Num.t
end

module With (S : Ops) : sig
  val from : S.Num.t -> S.pair list -> S.pair list
end
