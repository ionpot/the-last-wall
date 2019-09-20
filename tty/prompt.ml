open Convert
open Printf

let either a b x =
  if x then a else b

let sort_by_str to_str ls =
  List.sort (fun a b -> String.compare (to_str a) (to_str b)) ls

let echo f x = f x; x

let choose_from avlb str =
  Listx.filteri (fun i _ -> String.contains str (int2ichar i)) avlb

let choose_one avlb other str =
  match choose_from avlb str with
  | [] -> other
  | x :: _ -> x

let swap_empty ls = function
  | [] -> ls
  | ls -> ls

let indent = List.map (fun str -> "  " ^ str)
let indices = List.mapi (fun i str -> sprintf "%c) %s" (int2ichar i) str)

let highlight chosen to_str x =
  let str = to_str x in
  if List.mem x chosen
  then brackets str
  else str

let horizontal prefix = function
  | [] -> ()
  | ls -> Tty.pairln prefix (indices ls |> spaces)

let vertical prefix ls =
  Tty.writeln (prefix ^ ":");
  indices ls |> indent |> Tty.writelns

let ballista n avlb =
  sprintf "have %d ballista, build more? (max %d)" n avlb
  |> Tty.writeln;
  Tty.prompt_amount avlb

let barrage () =
  Tty.prompt_yn "arrow barrage? y/n"

let berserker avlb =
  Tty.writeln (sprintf "can train %d berserker" avlb);
  Tty.prompt_amount avlb

module Build (S : Game.State.S) = struct
  module Bonus = Game.Build_bonus.From(S)

  let add_from avlb ch out =
    try List.nth avlb (ichar2int ch) :: out
    with _ -> out

  let choose avlb ls =
    List.fold_right (add_from avlb) ls []
    |> Listx.dedupe_if (fun k -> not (Game.Build.multiple k))

  let from avlb t =
    let avlb' = sort_by_str bld2str avlb in
    List.map (fun kind ->
      let cost = Game.Build.cost_of kind Bonus.value in
      sprintf "%s [%s]" (bld2str kind) (res2str cost)) avlb'
    |> vertical "buildings available";
    Tty.prompt_chars "build?"
    |> choose avlb'
    |> echo (fun ls -> if ls <> []
      then List.map bld2str ls |> commas |> Tty.pairln "building")
end

let deity () =
  let ls = Game.Deity.list in
  let fns = [deity2str; deity_text] in
  List.map (fun d -> Listx.apply d fns |> commas) ls
  |> vertical "deities";
  Tty.prompt "choose"
  |> choose_one ls Game.Deity.empty
  |> echo (fun d -> Tty.spln (deity2str d) "will enlighten our path in this holy quest")

let dervish cap =
  Tty.writeln (sprintf "%d dervish available" cap);
  Tty.prompt_amount cap

let leader () =
  let ls = Game.Leader.kinds in
  List.map ldr2kind ls |> horizontal "leaders";
  Tty.prompt "choose"
  |> choose_one ls Game.Leader.(kind_of empty)

let new_leader ls =
  List.map ldr2full ls |> vertical "new leader";
  Tty.prompt "choose" |> choose_from ls

let knight cap =
  Tty.writeln (sprintf "can promote %d cavalry to knight" cap);
  Tty.prompt_amount cap

let mercs cap =
  Tty.writeln (sprintf "%d mercenaries available" cap);
  Tty.prompt_amount cap

let nations chosen =
  let ls = Game.Nation.kinds in
  List.map (highlight chosen nation2str) ls
  |> horizontal "nations";
  Tty.prompt "choose"
  |> choose_from ls
  |> Listx.pick_first Game.Nation.max_allowed
  |> swap_empty chosen

let ranger cap =
  Tty.writeln (sprintf "can promote %d dervish to ranger" cap);
  Tty.prompt_amount cap

let scout () =
  Tty.prompt_yn "send scouts? y/n"
  |> echo (fun x -> if x then Tty.writeln "scouts sent")

let templar cap =
  Tty.writeln (sprintf "can promote %d dervish to templar" cap);
  Tty.prompt_amount cap

let trade_nation () =
  let ls = Game.Nation.kinds in
  List.map nation2str ls
  |> horizontal "trade guild nation";
  Tty.prompt "choose"
  |> choose_one ls (List.hd ls)

let trade () =
  let nation = trade_nation () in
  let boost, certain = Game.Nation.(Boost nation, Certain nation) in
  [boost; certain]
  |> List.map trade2str
  |> horizontal "trade type";
  if Tty.prompt_int "choose" = 2 then certain else boost
  |> echo (fun x -> Tty.pairln "chosen" (trade2str x))

let volunteers cap =
  Tty.writeln (sprintf "%d volunteers want to join" cap);
  Tty.prompt_amount cap
