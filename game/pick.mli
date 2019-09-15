module type Num = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module Int : Num with type t = int
module Float : Num with type t = float

module type Ops = sig
  module Cap : Num
  module Map : Map.S
  module Type : Num
  type step = Cap.t * Type.t
  val choose : Type.t Map.t -> Map.key
  val roll : Map.key -> Cap.t -> Type.t Map.t -> step
end

module With (S : Ops) : sig
  val from : S.Cap.t -> S.Type.t S.Map.t -> S.Type.t S.Map.t -> S.Type.t S.Map.t
end
