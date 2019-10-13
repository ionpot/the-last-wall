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

let barrage_train ok cost =
  let sup = sup2str cost in
  if ok
  then sprintf "train archers for %s? y/n" sup |> Tty.prompt_yn
  else begin
    sprintf "need %s to train archers" sup |> Tty.writeln;
    false
  end

let berserker avlb =
  Tty.writeln (sprintf "can train %d berserker" avlb);
  Tty.prompt_amount avlb

module Build = struct
  module Map = Game.Build.Map

  let add_from avlb ch out =
    try List.nth avlb (ichar2int ch) :: out
    with _ -> out

  let choose avlb ls =
    List.fold_right (add_from avlb) ls []
    |> Listx.dedupe_if (fun k -> not (Game.Build.is_multiple k))

  let from (_, avlb) =
    let kinds = Map.bindings avlb |> List.map fst |> sort_by_str bld2str in
    let to_str kind =
      Map.find kind avlb |> res2str |> sprintf "%s [%s]" (bld2str kind)
    in
    List.map to_str kinds
    |> vertical "buildings available";
    Tty.prompt_chars "build?"
    |> choose kinds
    |> echo (fun ls ->
        List.map bld2str ls |> commas |> Tty.ifpairln "building")
    |> (fun chosen -> chosen, avlb)
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

module Nations (S : Game.State.S) = struct
  module Support = Game.Support.Roll(S)

  let to_str kind =
    Support.chance_of kind
    |> S.Nation.return
    |> percent2intstr
    |> sprintf "%s (%s)" (nation2str kind)

  let from chosen =
    let ls = Game.Nation.kinds in
    List.map (highlight chosen to_str) ls
    |> horizontal "nations";
    Tty.prompt "choose"
    |> choose_from ls
    |> Listx.pick_first Game.Nation.max_allowed
    |> swap_empty chosen
end

let ranger cap =
  Tty.writeln (sprintf "can promote %d dervish to ranger" cap);
  Tty.prompt_amount cap

let scout () =
  Tty.prompt_yn "send scouts? y/n"
  |> echo (fun x -> if x then Tty.writeln "scouts sent")

let templar cap =
  Tty.writeln (sprintf "can promote %d dervish to templar" cap);
  Tty.prompt_amount cap

let trade () =
  let ls = Game.Nation.kinds in
  List.map nation2str ls
  |> horizontal "trade guild nation";
  Tty.prompt "choose"
  |> choose_one ls (List.hd ls)
  |> (fun nat -> Tty.writeln (trade2str nat); Some nat)

let volunteers cap =
  Tty.writeln (sprintf "%d volunteers want to join" cap);
  Tty.prompt_amount cap
