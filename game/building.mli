type t = Market | Stable | Tavern | Temple
type status =
  | Absent
  | Waiting of Resource.t
  | Built
  | Ready
type report = (t * status) list
type state

val initial : state
val tlist : t list

val build : t list -> state -> state
val cost_of : t -> Resource.t
val draw_supp : Resource.t -> state -> Resource.t * state
val is_ready : t -> state -> bool
val report_of : state -> report
val status_of : t -> state -> status
val tick : Resource.t -> state -> Resource.t * state
