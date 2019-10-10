type 'a t = 'a * 'a

module Int : sig
  type nonrec t = int t
  val add : int -> t -> t
end
