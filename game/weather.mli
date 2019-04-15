type degree = Light | Heavy
type t =
  | Sunny
  | Clear
  | Cloudy
  | Wind
  | Rain of degree
  | Snow of degree

val empty : t

val is_bad : t -> bool
val pick : Month.t -> t
