module type From = sig
  type t
  val empty : t
end

module type FromBit = From with type t := bool
module type FromNum = From with type t := int

module type Val = sig
  type t
  val get : unit -> t
  val peek : (t -> unit) -> unit
  val return : (t -> 'a) -> 'a
  val map : (t -> t) -> unit
  val set : t -> unit
end

module type Bit = sig
  include Val with type t := bool
  val clr : unit -> unit
  val flip : unit -> unit
  val set : unit -> unit
end

module type Num = sig
  include Val with type t := int
  val add : int -> unit
  val sub : int -> unit
  val clr : unit -> unit
  val deduce : int -> int
  val deduce_from : int -> int
end

module From (M : From) = struct
  type t = M.t
  let x = ref M.empty
  let get () = !x
  let peek f = f !x
  let return f = f !x
  let set v = x := v
  let map f = set (f !x)
end

module Of (M : From) : Val = struct
  include From(M)
end

module Bit (M : FromBit) : Bit = struct
  include From(struct
    type t = bool
    let empty = M.empty
  end)
  let clr () = set false
  let flip () = map not
  let set () = set true
end

module Num (M : FromNum) : Num = struct
  include From(struct
    type t = int
    let empty = M.empty
  end)
  let add i = map ((+) i)
  let sub i = map ((-) i)
  let clr () = set 0
  let deduce i =
    let a, b = Number.deduce (get ()) i in
    set a; b
  let deduce_from i =
    let a, b = return (Number.deduce i) in
    set b; a
end

module BitSet : FromBit = struct let empty = true end
module BitClr : FromBit = struct let empty = false end

module Zero : FromNum = struct let empty = 0 end

let of_num x =
  let module M : FromNum = struct
    let empty = x
  end in
  (module Num(M) : Num)
