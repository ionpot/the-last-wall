module type S = sig
  type event_def
  val first : unit -> event_def
  val apply : event_def -> unit
  val next : event_def -> event_def
end
