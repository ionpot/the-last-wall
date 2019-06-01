open Game

module type S = sig
  val cavalry : Defs.count -> unit
  val dervish : unit -> unit
  val enemies : unit -> unit
  val res : unit -> unit
  val units : unit -> unit
end

module With (S : State.S) = struct
  open Printf

  let cavalry n =
    S.Units.return Units.(count Cavalry)
    |> sprintf "%d cavalry arrive, %d total" n
    |> Tty.writeln

  let dervish () =
    S.Units.return Units.(count Dervish)
    |> sprintf "%d dervish in total"
    |> Tty.writeln

  let enemies () =
    S.Enemy.return Convert.units2str
    |> Tty.pairln "enemies"

  let res () =
    let m = S.Units.return Units.workforce |> truncate in
    let s = S.Supply.get () in
    Convert.([sup2str s; manp2str m] |> commas)
    |> Tty.pairln "status"

  let units () =
    S.Units.return Units.power
    |> truncate
    |> Convert.manp2str
    |> sprintf "%s -> %s" (S.Units.return Convert.units2str)
    |> Tty.pairln "status"
end
