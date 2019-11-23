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

let choose_with enabled ls str =
  let f i (e, _) = if e then String.contains str (int2ichar i) else false in
  List.combine enabled ls
  |> Listx.filteri f
  |> List.map snd

let swap_empty ls = function
  | [] -> ls
  | ls -> ls

let indent = List.map (fun str -> "  " ^ str)

let to_index enabled i str =
  if enabled
  then sprintf "%c) %s" (int2ichar i) str
  else sprintf "   %s" str

let indices = List.mapi (to_index true)

let indices_with enabled =
  List.mapi (fun i str -> to_index (List.nth enabled i) i str)

let horizontal prefix = function
  | [] -> ()
  | ls -> Tty.pairln prefix (indices ls |> spaces)

let vertical prefix ls =
  Tty.writeln (prefix ^ ":");
  indices ls |> indent |> Tty.writelns

let vertical_with enabled prefix ls =
  Tty.writeln (prefix ^ ":");
  indices_with enabled ls |> indent |> Tty.writelns

let one_nation str =
  let ls = Game.Nation.kinds in
  List.map nation2str ls
  |> horizontal str;
  Tty.prompt "choose"
  |> choose_one ls (List.hd ls)

let ballista avlb =
  sprintf "can build %d ballista" avlb
  |> Tty.writeln;
  Tty.prompt_amount avlb

let barracks () =
  let chosen nat = sprintf "%s chosen for barracks" (nation2str nat) in
  one_nation "barracks nation"
  |> (fun nat -> Tty.writeln (chosen nat); Some nat)

let barrage status =
  if status = Game.Barrage.Available
  then Tty.prompt_yn "arrow barrage? y/n"
  else false

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

  let from nat (_, avlb) =
    let bldstr = bld2str nat in
    let kinds =
      Map.bindings avlb |> List.map fst |> sort_by_str bldstr in
    let to_str kind =
      Map.find kind avlb |> res2str |> sprintf "%s [%s]" (bldstr kind)
    in
    List.map to_str kinds
    |> vertical "buildings available";
    Tty.prompt_chars "build?"
    |> choose kinds
    |> echo (fun ls ->
        List.map bldstr ls |> commas |> Tty.ifpairln "building")
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

module Leader = struct
  let first () =
    let ls = Game.Leader.kinds in
    List.map ldr2kind ls |> horizontal "leaders";
    Tty.prompt "choose"
    |> choose_one ls Game.Leader.(kind_of empty)

  let cant_afford () =
    Tty.writeln "cannot afford new leader"

  let pair pairs =
    let ((a, a_ok), (b, b_ok)) = pairs in
    let to_pairs = function
      | [] -> (a, false), (b, false)
      | x :: _ -> (a, x = a), (b, x = b)
    in
    let ls = [a; b] in
    let enabled = [a_ok; b_ok] in
    List.map ldr2full ls |> vertical_with enabled "new leader";
    match a_ok, b_ok with
    | false, false -> cant_afford (); pairs
    | _ -> Tty.prompt "choose" |> choose_with enabled ls |> to_pairs
end

let mercs cap =
  Tty.writeln (sprintf "%d mercenaries available" cap);
  Tty.prompt_amount cap

module Nations (S : Game.State.S) = struct
  module Set = Game.Nation.Set
  module Support = Game.Support.Roll(S)

  let highlight chosen to_str x =
    let str = to_str x in
    if Set.mem x chosen
    then brackets str
    else str

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
    |> swap_empty (Set.elements chosen)
    |> Set.of_list
end

let promote kind cap =
  let k = Game.Units.Promote.needs kind in
  sprintf "can promote %s to %s"
    (unit_n2str cap k) (unit2str kind)
  |> Tty.writeln;
  Tty.prompt_amount cap

let scout () =
  Tty.prompt_yn "send scouts? y/n"
  |> echo (fun x -> if x then Tty.writeln "scouts sent")

let sodistan cap =
  Tty.writeln (sprintf "can convert %s from sodistan" (sup2str cap));
  Tty.prompt_amount cap

let temple cap =
  unit_n2str cap Game.Units.Men
  |> sprintf "temple can aid with %s"
  |> Tty.writeln;
  Tty.prompt_amount cap

let trade () =
  one_nation "trade guild nation"
  |> (fun nat -> Tty.writeln (trade2str nat); Some nat)

let volunteers cap =
  Tty.writeln (sprintf "%d volunteers want to join" cap);
  Tty.prompt_amount cap
