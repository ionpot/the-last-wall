open Convert
open Game
open Printf

module S = State.Make( )
module I = Initial.Make(S)
module R = Regular.Make(S)

let read_char () =
  match read_line () with
  | "" -> '0'
  | x -> String.get x 0

let prompt_deity () =
  print_string "deities: ";
  Deity.t_list
  |> List.map (fun d -> (deity2char d, deity2str d))
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)
  |> List.iter (fun (c, s) -> printf " %c) %s" c s);
  print_string "\nchoose> ";
  read_char () |> char2deity

let deity_chosen d =
  printf "%s chosen\n" (deity2str d)

let nat_str n chosen =
  let str = nation2str n in
  if List.mem n chosen
  then sprintf "[%s]" str
  else sprintf "%s" str

let prompt_nations chosen =
  print_string "nations: ";
  Nation.t_list
  |> List.map (fun n -> (nation2char n, nat_str n chosen))
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)
  |> List.iter (fun (c, s) -> printf " %c) %s" c s);
  print_string "\nchoose> ";
  read_line () |> line2nats chosen

let nations_chosen ns =
  List.map nation2str ns
  |> String.concat ", "
  |> printf "chosen: %s\n"

let prompt_scouting () =
  print_string "send scouts? y/n> ";
  read_char () |> yn2bool

let scouting_chosen s =
  if s then print_string "scouts sent\n"

let print_leader str x =
  leader2str x
  |> printf "%s %s\n" str

let print_support ls =
  let f = function
    | Some x -> res2str x
    | None -> "nothing"
  in
  List.map (fun (nat, res) -> (nation2str nat, f res)) ls
  |> List.iter (fun (nat, res) -> printf "%s sent %s\n" nat res)

let print_status () =
  S.get_res ()
  |> res2str
  |> printf "status: %s\n"

let r_event evt =
  let open Regular in
  match evt with
  | Attack ls ->
      enemies2str ls |> printf "attack: %s\n"; None
  | Blessing res ->
      res2str res |> printf "blessing: %s\n"; None
  | Casualty mp ->
      manp2str mp |> printf "casualty: %s\n"; None
  | End -> None
  | LeaderDied x ->
      print_leader "leader died, was" x; None
  | LeaderLvup x ->
      print_leader "leader is now" x; None
  | LeaderNew x ->
      print_leader "new leader:" x; None
  | Nations ls ->
      let ns = prompt_nations ls in
      nations_chosen ns;
      Some (Nations ns)
  | ScoutReport ls ->
      report2str ls
      |> printf "seen: %s\n"; None
  | ScoutSumReport x ->
      let (count, str) = sum2str x in
      printf "seen: about %d (%s)\n" count str; None
  | ScoutsSent res ->
      res2str res
      |> printf "scouts cost: %s\n"; None
  | SendScouts _ ->
      let s = prompt_scouting () in
      scouting_chosen s;
      Some (SendScouts s)
  | Starvation mp ->
      manp2str mp |> printf "starvation: %s\n"; None
  | Support ls ->
      print_support ls; None
  | Turn x ->
      printf "\nturn %d\n" x; None
  | Upkeep sup ->
      sup2str sup |> printf "upkeep: %s\n"; None

let rec r_loop evt =
  if evt <> Regular.End
  then begin
    let e =
      match r_event evt with
      | Some x -> x
      | None -> evt
    in
    let x = R.next e in
    print_status ();
    r_loop x
  end

let rec i_loop evt =
  let next_with e = I.next e |> i_loop in
  let next () = next_with evt in
  let open Initial in
  match evt with
  | Deity _ ->
      let d = prompt_deity () in
      deity_chosen d;
      next_with (Deity d)
  | End ->
      R.first () |> r_loop
  | Nations x ->
      let ns = prompt_nations x in
      nations_chosen ns;
      next_with (Nations ns)
  | NewLeader x ->
      print_leader "new leader:" x;
      next ()
  | SendScouts _ ->
      let s = prompt_scouting () in
      scouting_chosen s;
      next_with (SendScouts s)
  | Starting res ->
      printf "starting: %s\n" (res2str res);
      next ()
  | Support ls ->
      print_support ls;
      next ()

let () =
  Random.self_init ();
  I.first () |> i_loop
