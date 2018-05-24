type leader = Alive | Dead
type manpower = int
type nation = Tulron | Sodistan | Hekatium | Numendor | Clan
type supply = int
type turn = int

let nation_list =
  [Tulron; Sodistan; Hekatium; Numendor; Clan]

module type Phase = sig
  type event_def
  val first : unit -> event_def
  val next : event_def -> event_def
end
