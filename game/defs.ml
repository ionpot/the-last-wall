type manpower = int
type supply = int
type turn = int

module type Phase = sig
  type event_def
  val first : unit -> event_def
  val next : event_def -> event_def
end
