type deity = Elanis | Sekrefir | Sitera | None
type leader = Alive | Dead
type manpower = int
type nation = Tulron | Sodistan | Hekatium | Numendor | Clan
type supply = int

module type Resource_t = sig
  type t
  val make : unit -> t
  val manp : t -> manpower
  val supp : t -> supply
  val add : t -> t -> t
  val add_m : manpower -> t -> t
  val add_s : supply -> t -> t
end

module Resource : Resource_t = struct
  open Pair
  type t = (manpower * supply)
  let make () = (0, 0)
  let manp r = fst r
  let supp r = snd r
  let add a b = a ++ b
  let add_m x r = x +< r
  let add_s x r = x +> r
end

type resource = Resource.t

module type Phase = sig
  type _ event
  type input
  val first : 'a event
  val apply : 'a event -> 'a -> unit
  val get_deity : unit -> deity
  val get_leader : unit -> leader
  val get_manpower : unit -> manpower
  val get_nations : unit -> nation list
  val get_resources : unit -> resource
  val get_supplies : unit -> supply
  val next : 'a event -> 'b event
  val outcome_of : 'a event -> 'a
  val set : input -> unit
end

module type Transition = sig
  val deity : deity
  val leader : leader
  val nations : nation list
  val resource : resource
end
