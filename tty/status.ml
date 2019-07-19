open Game

module type S = sig
  val cavalry : Defs.count -> unit
  val dervish : unit -> unit
  val enemies : unit -> unit
  val enemies_remaining : unit -> unit
  val new_leader : Leader.t list -> unit
  val ranger : unit -> unit
  val res : unit -> unit
  val templar : unit -> unit
  val units : unit -> unit
end

module With (S : State.S) = struct
  open Printf

  let count kind =
    Units.count kind |> S.Units.return

  let cavalry n =
    count Units.Cavalry
    |> sprintf "%d cavalry arrive, %d total" n
    |> Tty.writeln

  let dervish () =
    count Units.Dervish
    |> sprintf "%d dervish in total"
    |> Tty.writeln

  let total kinds =
    kinds
    |> List.map (fun k -> k, count k)
    |> Convert.unit_pairs2str
    |> Tty.pairln "total"

  let ranger () =
    total Units.([Ranger; Dervish])

  let templar () =
    total Units.([Templar; Dervish])

  let print_enemies prefix =
    S.Enemy.return Units.report
    |> Convert.unit_pairs2str
    |> Tty.ifpairln prefix

  let enemies () =
    print_enemies "enemies"

  let enemies_remaining () =
    print_enemies "enemies remaining"

  let res () =
    let m = S.Units.return Units.workforce |> truncate in
    let s = S.Supply.get () in
    Convert.([sup2str s; manp2str m] |> commas)
    |> Tty.pairln "status"

  let new_leader = function
    | [] -> ()
    | ldr :: _ ->
        sprintf "%s chosen" (Convert.ldr2first ldr)
        |> Tty.writeln;
        res ()

  let units () =
    S.Units.return Convert.units2mnpstr
    |> Tty.pairln "status"
end