module type Of = sig
  type t
  val empty : t
end

module type OfBit = Of with type t = bool
module type OfNum = Of with type t = int

module type Val = sig
  type t
  val get : unit -> t
  val peek : (t -> unit) -> unit
  val return : (t -> 'a) -> 'a
  val map : (t -> t) -> unit
  val set : t -> unit
end

module type Bit = sig
  include Val with type t = bool
  val clr : unit -> unit
  val flip : unit -> unit
  val set : unit -> unit
end

module type Num = sig
  include Val with type t = int
  val add : t -> unit
  val sub : t -> unit
  val clr : unit -> unit
  val deduce : t -> t
  val deduce_from : t -> t
end

module Of (M : Of) = struct
  type t = M.t
  let x = ref M.empty
  let get () = !x
  let peek f = f !x
  let return f = f !x
  let set v = x := v
  let map f = set (f !x)
end

module Bit (M : OfBit) = struct
  include Of(M)
  let clr () = set false
  let flip () = map not
  let set () = set true
end

module Num (M : OfNum) = struct
  include Of(M)
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

module BitSet = struct
  type t = bool
  let empty = true
end

module BitClr = struct
  type t = bool
  let empty = false
end

module Zero = struct
  type t = int
  let empty = 0
end

let of_num x =
  let module M = struct
    type t = int
    let empty = x
  end in
  (module Num(M) : Num)
