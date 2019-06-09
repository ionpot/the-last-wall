open Defs

val invalid : count list -> 'a

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
  val roll : pair -> power * Num.t
  val trim : power -> pair -> Num.t
end

module With (S : Ops) : sig
  val from : power -> S.pair list -> S.pair list
end
