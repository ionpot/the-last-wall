module type Num = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module Int : Num with type t = int
module Float : Num with type t = float

module type OpsBase = sig
  module Cap : Num
  module Map : Map.S
  module Type : Num
  type map = Type.t Map.t
end

module type Ops = sig
  include OpsBase
  type step = Cap.t * Type.t
  val choose : map -> Map.key
  val roll : Map.key -> Cap.t -> map -> step
end

module type OpsAcc = sig
  include OpsBase
  type acc
  type step = acc * Cap.t * Type.t
  val choose : map -> Map.key
  val roll : acc -> Map.key -> Cap.t -> map -> step
end

module With (S : Ops) : sig
  val from : S.Cap.t -> S.map -> S.map -> S.map
end

module WithAcc (S : OpsAcc) : sig
  val from : S.acc -> S.Cap.t -> S.map -> S.map -> S.acc * S.map * S.map
end
