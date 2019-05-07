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

let barrage () =
  Tty.prompt_yn "arrow barrage? y/n"

let build avlb t =
  let avlb' = sort_by_str bld2str avlb in
  List.map (fun kind ->
    let cost = Game.Build.cost_of kind t in
    sprintf "%s [%s]" (bld2str kind) (res2str cost)) avlb'
  |> vertical "buildings available";
  Tty.prompt "build?"
  |> choose_from avlb'
  |> echo (fun ls -> if ls <> []
    then List.map bld2str ls |> commas |> Tty.pairln "building")

let deity () =
  let ls = Game.Deity.list in
  List.map deity2str ls |> horizontal "deities";
  Tty.prompt "choose"
  |> choose_one ls Game.Deity.empty
  |> echo (fun d -> Tty.spln (deity2str d) "chosen")

let dervish cap =
  Tty.writeln (sprintf "%d dervish available" cap);
  Tty.prompt_yn "accept? y/n"
  |> either cap 0

let leader () =
  let ls = Game.Leader.kinds in
  List.map ldr2kind ls |> horizontal "leaders";
  Tty.prompt "choose"
  |> choose_one ls Game.Leader.(kind_of empty)

let mercs cap =
  Tty.writeln (sprintf "%d mercenaries available" cap);
  Tty.prompt_yn "accept? y/n"
  |> either cap 0

let nations chosen =
  let ls = Game.Nation.kinds in
  List.map (highlight chosen nation2str) ls
  |> horizontal "nations";
  Tty.prompt "choose"
  |> choose_from ls
  |> Listx.pick_first Game.Nation.max_allowed
  |> swap_empty chosen

let scout () =
  Tty.prompt_yn "send scouts? y/n"
  |> echo (fun x -> if x then Tty.writeln "scouts sent")

let trade_nation () =
  let ls = Game.Nation.kinds in
  List.map nation2str ls
  |> horizontal "trade guild nation";
  Tty.prompt "choose"
  |> choose_one ls (List.hd ls)

let trade () =
  let nation = trade_nation () in
  ["boost support"; "certain support"]
  |> horizontal "trade type";
  if Tty.prompt_int "choose" = 2
  then Game.Nation.Certain nation
  else Game.Nation.Boost nation