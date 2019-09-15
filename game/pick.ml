module type Num = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module Int = struct
  type t = int let zero = 0 let add = (+) let sub = (-)
end

module Float = struct
  type t = float let zero = 0. let add = (+.) let sub = (-.)
end

module type Ops = sig
  module Cap : Num
  module Map : Map.S
  module Type : Num
  type step = Cap.t * Type.t
  val choose : Type.t Map.t -> Map.key
  val roll : Map.key -> Cap.t -> Type.t Map.t -> step
end

module With (S : Ops) = struct
  let add key v output =
    let f = function
      | Some x -> Some (S.Type.add v x)
      | None -> Some v
    in
    S.Map.update key f output

  let sub key v input =
    let f = function
      | Some x ->
          if x > v then Some (S.Type.sub x v) else None
      | None -> None
    in
    S.Map.update key f input

  let rec from cap input output =
    if cap <= S.Cap.zero || S.Map.is_empty input
    then output
    else
      let key = S.choose input in
      let cap', n = S.roll key cap input in
      from (S.Cap.sub cap cap') (sub key n input) (add key n output)
end
