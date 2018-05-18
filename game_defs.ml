type deity = Elanis | Sekrefir | Sitera | NoDeity
type enemy = Skeleton | Orc | Demon
type leader = Alive | Dead
type manpower = int
type nation = Tulron | Sodistan | Hekatium | Numendor | Clan
type supply = int
type turn = int

let enemy_list =
  [Skeleton; Orc; Demon]

let nation_list =
  [Tulron; Sodistan; Hekatium; Numendor; Clan]

module type Phase = sig
  type event
  val first : unit -> event
  val next : event -> event
end
