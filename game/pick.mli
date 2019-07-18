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
  module Total : Num
  type key
  type pair = key * Num.t
  val choose : pair list -> pair
  val roll : pair -> Total.t * Num.t
  val trim : Total.t -> pair -> Num.t
end

module With (S : Ops) : sig
  val from : S.Total.t -> S.pair list -> S.pair list
end
