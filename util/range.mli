type 'a t = 'a * 'a

module Int : sig
  type nonrec t = int t
  val add : int -> t -> t
  val combine : t -> t -> t
  val combine_if : bool -> t -> t -> t
  val times : int -> t -> t
end
