type deity = Elanis | Sekrefir | Sitera | None
type leader = Alive | Dead
type manpower = int
type nation = Tulron | Sodistan | Hekatium | Numendor | Clan
type supply = int
type outcome =
  | Leader of leader
  | Manpower of manpower
  | Supplies of supply

module type Phase = sig
  type event
  type input
  type _ query =
    | Leader : leader query
    | Manpower : manpower query
    | Nations : nation list query
    | Supplies : supply query
  val get : type a. a query -> a
  val next : unit -> event
  val set : input -> unit
  val apply : event -> outcome list
end

module type Initial = Phase with
  type event =
    | Blessing
    | Deity
    | Nation
    | Support
    | Starting
    | Next
  and
  type input =
    | Deity of deity
    | Nations of nation list

module type Regular = Phase

module Make( ) : Initial
module Next(X : Initial) : Regular
