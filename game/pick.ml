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

module Base (S : OpsBase) = struct
  let add key v output =
    let f = function
      | Some x -> Some (S.Type.add v x)
      | None -> Some v
    in
    S.Map.update key f output

  let check cap input =
    cap <= S.Cap.zero || S.Map.is_empty input

  let sub key v input =
    let f = function
      | Some x ->
          if x > v then Some (S.Type.sub x v) else None
      | None -> None
    in
    S.Map.update key f input
end

module With (S : Ops) = struct
  module Base = Base(S)

  let rec from cap input output =
    if Base.check cap input
    then output
    else
      let key = S.choose input in
      let cap', n = S.roll key cap input in
      from (S.Cap.sub cap cap')
        (Base.sub key n input)
        (Base.add key n output)
end

module WithAcc (S : OpsAcc) = struct
  module Base = Base(S)

  let rec from acc cap input output =
    if Base.check cap input
    then acc, output
    else
      let key = S.choose input in
      let acc', cap', n = S.roll acc key cap input in
      from acc' (S.Cap.sub cap cap')
        (Base.sub key n input)
        (Base.add key n output)
end
