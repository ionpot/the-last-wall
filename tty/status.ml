open Game

module type S = sig
  val berserker : unit -> unit
  val cavalry : Defs.count -> unit
  val dervish : unit -> unit
  val enemies : unit -> unit
  val facilities : Direct.Facilities.t -> unit
  val leader : unit -> unit
  val new_leader : Input.LeaderNew.t -> unit
  val ranger : unit -> unit
  val res : unit -> unit
  val templar : unit -> unit
  val units : unit -> unit
end

module With (S : State.S) = struct
  open Printf

  let count kind =
    Units.count kind |> S.Units.return

  let berserker () =
    count Units.Berserker
    |> sprintf "%d berserker in total"
    |> Tty.writeln

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

  let res () =
    let base = S.Bonus.return Power.base in
    let units =  S.Units.get () in
    let m = Power.of_units units base |> truncate in
    let s = S.Supply.get () in
    let w = Convert.units2work base units in
    Printf.sprintf "status: %s, %s (%s)"
      (Convert.sup2str s)
      (Convert.manp2str m)
      (Convert.work2str w)
    |> Tty.writeln

  let facilities t =
    if Convert.(facs2clean t |> facs2bool)
    then res ()

  let leader () =
    S.Leader.return Convert.ldr2full
    |> Tty.pairln "leader"

  let new_leader =
    let chosen ldr =
      sprintf "%s chosen" (Convert.ldr2first ldr)
      |> Tty.writeln
    in
    function
      | (ldr, true), _
      | _, (ldr, true) -> chosen ldr; res ()
      | (_, false), (_, false) -> ()

  let units () =
    let base = S.Bonus.return Power.base in
    S.Units.return (Convert.units2mnpstr base)
    |> Tty.pairln "status"
end
