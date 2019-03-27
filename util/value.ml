module type From = sig
  type t
  val empty : t
end

module type FromBit = From with type t := bool
module type FromNum = From with type t := int

module type S = sig
  type t
  val check : (t -> bool) -> bool
  val get : unit -> t
  val map : (t -> t) -> unit
  val peek : (t -> unit) -> unit
  val return : (t -> 'a) -> 'a
  val set : t -> unit
end

module type Bit = sig
  include S with type t := bool
  val clr : unit -> unit
  val flip : unit -> unit
  val set : unit -> unit
  val set_to : bool -> unit
end

module type Num = sig
  include S with type t := int
  val add : int -> unit
  val sub : int -> unit
  val clr : unit -> unit
  val next : unit -> int
  val ptv : unit -> bool
  val deduce : int -> int
  val deduce_from : int -> int
  val take : int -> int
  val zero : unit -> bool
end

module From (M : From) = struct
  type t = M.t
  let x = ref M.empty
  let get () = !x
  let peek f = f !x
  let check f = f !x
  let return f = f !x
  let set v = x := v
  let map f = set (f !x)
end

module Of (M : From) : S = struct
  include From(M)
end

module Bit (M : FromBit) : Bit = struct
  include From(struct
    type t = bool
    let empty = M.empty
  end)
  let clr () = set false
  let flip () = map not
  let set_to x = set x
  let set () = set true
end

module Num (M : FromNum) : Num = struct
  include From(struct
    type t = int
    let empty = M.empty
  end)
  let add i = map ((+) i)
  let sub i = map (fun x -> x - i)
  let clr () = set 0
  let ptv () = return ((<) 0)
  let next () = return ((+) 1)
  let deduce i =
    let a, b = Number.deduce (get ()) i in
    set a; b
  let deduce_from i =
    let a, b = return (Number.deduce i) in
    set b; a
  let take i =
    let a, b = Number.take (get ()) i in
    set a; b
  let zero () = return ((=) 0)
end

module BitSet : FromBit = struct let empty = true end
module BitClr : FromBit = struct let empty = false end

module Zero : FromNum = struct let empty = 0 end

let of_num x =
  let module M : FromNum = struct
    let empty = x
  end in
  (module Num(M) : Num)
