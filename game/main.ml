type event =
  | Ph1 of Phase1.event
  | Ph2 of Phase2.event
  | Ph3 of Phase3.event
  | End

module type S = sig
  val first : unit -> event
  val next : event -> event
end

module Make (M : State.S) : S = struct
  module P1 = Phase1.Make(M)
  module P2 = Phase2.Make(M)
  module P3 = Phase3.Make(M)

  let apply = function
    | Ph1 x -> P1.apply x
    | Ph2 x -> P2.apply x
    | Ph3 x -> P3.apply x
    | End -> ()

  let next_of = function
    | Ph2 Phase2.Defeat
    | Ph3 Phase3.Defeat
    | End -> End
    | Ph1 Phase1.End
    | Ph3 Phase3.End -> Ph2 (P2.first ())
    | Ph2 Phase2.End -> Ph3 (P3.first ())
    | Ph1 x -> Ph1 (P1.next x)
    | Ph2 x -> Ph2 (P2.next x)
    | Ph3 x -> Ph3 (P3.next x)

  let first () = Ph1 (P1.first ())
  let next ev = apply ev; next_of ev
end
