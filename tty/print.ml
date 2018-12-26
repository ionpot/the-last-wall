open Convert
open Game
open Printf

let bld_chosen ls =
  if ls <> [] then
    let str = List.map bld2str ls |> String.concat ", " in
    Tty.pairln "building" str

let bld_supp s =
  Tty.pairln "construction" (sup2str s)

let defense men cav ldr =
  let str = defense2str men cav ldr in
  Tty.pairln "defense" str;
  if Cavalry.too_many cav men
  then Tty.writeln "too many cavalry, defense reduced"

let deity_chosen d =
  Tty.writeln @@ sprintf "%s chosen" (deity2str d)

let leader str ldr =
  Tty.writeln @@ sprintf "%s %s" str (leader2str ldr)

let leader_chosen = function
  | Some ldr -> leader2str ldr |> Tty.pairln "leader"
  | None -> ()

let nations_chosen ns =
  let f = nation2char in
  let str =
    ns
    |> List.sort (fun a b -> Char.compare (f a) (f b))
    |> List.map nation2str
    |> String.concat ", "
  in
  if str <> ""
  then Tty.pairln "chosen" str

let queued ls =
  List.map (Pair.map (bld2str, res2str)) ls
  |> List.map (fun (b, r) -> sprintf "%s (%s)" b r)
  |> String.concat ", "
  |> Tty.pairln "unfinished"

let scouting_chosen s =
  if s then Tty.writeln "scouts sent"

let support ls =
  let f = function
    | Some x -> res2str x
    | None -> "nothing"
  in
  ls
  |> List.map (fun (nat, res) -> (nation2str nat, f res))
  |> List.map (fun (nat, res) -> sprintf "%s sent %s" nat res)
  |> List.iter Tty.writeln
