module type From = sig
  type t
  val empty : t
end

module type FromBit = From with type t = bool
module type FromNum = From with type t = int

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
  include S with type t = bool
  val clr : unit -> unit
  val flip : unit -> unit
  val set : unit -> unit
  val set_to : t -> unit
end

module type Num = sig
  include S with type t = int
  val add : t -> unit
  val sub : t -> unit
  val clr : unit -> unit
  val is_zero : unit -> bool
  val next : unit -> t
  val ptv : unit -> bool
  val deduce : t -> t
  val deduce_from : t -> t
  val take : t -> t
end

module From (M : From) : S with type t = M.t = struct
  type t = M.t
  let x = ref M.empty
  let get () = !x
  let peek f = f !x
  let check f = f !x
  let return f = f !x
  let set v = x := v
  let map f = set (f !x)
end

module Bit (M : FromBit) : Bit = struct
  include From(M)
  let clr () = set false
  let flip () = map not
  let set_to x = set x
  let set () = set true
end

module Num (M : FromNum) : Num = struct
  include From(M)
  let add i = map ((+) i)
  let sub i = map (fun x -> x - i)
  let clr () = set 0
  let is_zero () = return ((=) 0)
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
end

module True = struct type t = bool let empty = true end
module False = struct type t = bool let empty = false end

module Zero = struct type t = int let empty = 0 end

let of_num x =
  (module Num(struct
    type t = int
    let empty = x
  end) : Num)
