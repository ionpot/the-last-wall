module type From = sig
  type t
  val empty : t
end

module type FromBit = From with type t = bool
module type FromFloat = From with type t = float
module type FromNum = From with type t = int

module type S = sig
  type t
  val check : (t -> bool) -> bool
  val clear : unit -> unit
  val empty : unit -> bool
  val get : unit -> t
  val is : t -> bool
  val map : (t -> t) -> unit
  val peek : (t -> unit) -> unit
  val return : (t -> 'a) -> 'a
  val set : t -> unit
end

module type Bit = sig
  include S with type t = bool
  val either : 'a -> 'a -> 'a
  val flip : unit -> unit
  val set : unit -> unit
  val set_to : t -> unit
end

module type Float = sig
  include S with type t = float
  val add : t -> unit
end

module type Num = sig
  include S with type t = int
  val add : t -> unit
  val deduce : t -> t
  val deduce_from : t -> t
  val has : t -> bool
  val next : unit -> t
  val ngv : unit -> bool
  val ptv : unit -> bool
  val sub : t -> unit
  val take : t -> t
  val zero : unit -> bool
end

module From (M : From) : S with type t = M.t = struct
  type t = M.t
  let x = ref M.empty
  let check f = f !x
  let clear () = x := M.empty
  let empty () = !x = M.empty
  let get () = !x
  let is t = t = !x
  let peek f = f !x
  let return f = f !x
  let set v = x := v
  let map f = set (f !x)
end

module Bit (M : FromBit) : Bit = struct
  include From(M)
  let clear () = set false
  let either a b = if get () then a else b
  let flip () = map not
  let set_to x = set x
  let set () = set true
end

module Float (M : FromFloat) : Float = struct
  include From(M)
  let add x = map ((+.) x)
end

module Num (M : FromNum) : Num = struct
  include From(M)
  let add i = map ((+) i)
  let deduce i =
    let a, b = Number.deduce (get ()) i in
    set a; b
  let deduce_from i =
    let a, b = return (Number.deduce i) in
    set b; a
  let has x = return ((<=) x)
  let next () = return ((+) 1)
  let ngv () = return ((>) 0)
  let ptv () = return ((<) 0)
  let sub i = map (fun x -> x - i)
  let take i =
    let a, b = Number.take (get ()) i in
    set a; b
  let zero () = is 0
end

module True = struct type t = bool let empty = true end
module False = struct type t = bool let empty = false end

module Zero = struct type t = int let empty = 0 end
module ZeroF = struct type t = float let empty = 0. end

let of_num x =
  (module Num(struct
    type t = int
    let empty = x
  end) : Num)
