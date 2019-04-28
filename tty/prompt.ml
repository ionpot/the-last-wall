open Convert
open Printf

let sort_by_str to_str ls =
  List.sort (fun a b -> String.compare (to_str a) (to_str b)) ls

let print_chosen str =
  Tty.pairln "chosen" str

let echo_ls to_str ls =
  if ls <> [] then List.map to_str ls |> commas |> print_chosen;
  ls

let echo_one to_str chosen =
  print_chosen (to_str chosen);
  chosen

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
  |> echo_ls bld2str

let deity () =
  let ls = Game.Deity.list in
  List.map deity2str ls |> horizontal "deities";
  Tty.prompt "choose"
  |> choose_one ls Game.Deity.empty
  |> echo_one deity2str

let leader () =
  let ls = Game.Leader.kinds in
  let to_str = Leader.kind2str in
  List.map to_str ls |> horizontal "leaders";
  Tty.prompt "choose"
  |> choose_one ls Game.Leader.(kind_of empty)
  |> echo_one to_str

let mercs x =
  Tty.writeln (sprintf "%d mercenaries available" x);
  Tty.prompt_yn "accept? y/n"

let nations chosen =
  let ls = Game.Nation.kinds in
  List.map (highlight chosen nation2str) ls
  |> horizontal "nations";
  Tty.prompt "choose"
  |> choose_from ls
  |> Listx.pick_first Game.Nation.max_allowed
  |> swap_empty chosen
  |> echo_ls nation2str

let scout () =
  Tty.prompt_yn "send scouts? y/n"
