type deity = Elanis | Sekrefir | Sitera | None
type leader = Alive | Dead
type manpower = int
type nation = Tulron | Sodistan | Hekatium | Numendor | Clan
type supply = int

type resource = (manpower * supply)

module type Phase = sig
  type _ event
  type _ query =
    | Leader : leader query
    | Manpower : manpower query
    | Nations : nation list query
    | Supplies : supply query
  type input
  val first : 'a event
  val apply : type a. a event -> a -> unit
  val get : type a. a query -> a
  val next : 'a event -> 'b event
  val outcome_of : type a. a event -> a
  val set : input -> unit
end

module type Transition = sig
  val deity : deity
  val leader : leader
  val nations : nation list
  val resource : resource
end
