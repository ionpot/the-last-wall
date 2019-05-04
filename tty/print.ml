open Convert
open Game
open Printf

module Build = struct
  let status (ready, built, queued) =
    Tty.ifpairln "built" (bld_ls2str ready);
    Tty.ifpairln "finished" (bld_ls2str built);
    Tty.ifpairln "queued" (bld_q2str queued)

  let all t =
    status (Build.status t)

  let manp m =
    if m > 0 then
    Tty.spln "construction used" (manp2str m)

  let supply s =
    if s > 0 then
    Tty.spln "construction costs" (sup2str s)
end

let support ls =
  let f res =
    if res = Resource.empty then "nothing"
    else res2str res
  in
  ls
  |> List.map (fun (nat, res) -> (nation2str nat, f res))
  |> List.map (fun (nat, res) -> sprintf "%s sent %s" nat res)
  |> Tty.writelns

  (*
let defense men cav ldr =
  let str = defense2str men cav ldr in
  Tty.pairln "defense" str;
  if Cavalry.too_many cav men
  then Tty.writeln "too many cavalry, defense reduced"

let queued ls =
  List.map (Pair.map (bld2str, res2str)) ls
  |> List.map (fun (b, r) -> sprintf "%s (%s)" b r)
  |> commas
  |> Tty.pairln "unfinished"
  *)
