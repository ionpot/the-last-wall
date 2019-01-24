module Sp = Steps.Of(P.S)

module S = Game.State.Make (struct
  let deity = Game.Deity.Arnerula
  let leader = Game.Leader.Aristocrat
end)

let print = function
  | P.Defeat -> print_endline "defeat"
  | P.Scout x -> Printf.printf "scout: %B\n" x
  | P.Turn x -> Printf.printf "turn: %d\n" x

let handle = function
  | Some t ->
      print t;
      let module M = Sp.Apply(S) in M.value t
  | None -> ()

let rec loop = function
  | Some kind ->
      let module M = Sp.Make(S) in
      handle (M.value kind);
      Sp.next_of kind |> loop
  | None -> ()

let _ =
  S.set_scouting true;
  loop Sp.first

(*
type conditional = BldMp | Blt | InputBld | InputNats | Needs | LdrNew | Starvation | Defeat | Blessing | BldSup | Mercs
type direct = Turn | Upkeep | Report | Support

let p2 = [
  Do Turn;
  Check BldMp;
  Check Blt;
  Check Needs;
  Check LdrNew;
  Do Upkeep;
  CheckIf (Starvation, Defeat);
  Do Report;
  Do Nations;
  Check Blessing;
  Do Support;
  Do Build;
  Check BldSup;
  Check Mercs
]*)

