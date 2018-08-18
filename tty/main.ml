open Convert
open Game
open Printf

module S = State.Make( )
module I = Initial.Make(S)
module R = Regular.Make(S)

let nat_list =
  Nation.t_list
  |> List.map (fun n -> nation2char n, n)
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)

let prompt_deity () =
  Tty.write "deities:";
  Deity.t_list
  |> List.map (fun d -> (deity2char d, deity2str d))
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)
  |> List.map (fun (c, s) -> sprintf " %c) %s" c s)
  |> List.iter Tty.write;
  Tty.lnprompt_char "choose>" |> char2deity

let deity_chosen d =
  Tty.writeln @@ sprintf "%s chosen" (deity2str d)

let nat_str n chosen =
  let str = nation2str n in
  if List.mem n chosen
  then sprintf "[%s]" str
  else sprintf "%s" str

let filter_nats str =
  nat_list
  |> List.filter (fun (c, _) -> String.contains str c)

let new_nats chosen str =
  if str = "" then chosen else
    match filter_nats str with
    | [] -> chosen
    | ls -> List.map snd ls

let prompt_nations chosen =
  Tty.write "nations:";
  nat_list
  |> List.map (fun (c, n) -> c, nat_str n chosen)
  |> List.map (fun (c, s) -> sprintf " %c) %s" c s)
  |> List.iter Tty.write;
  Tty.lnprompt "choose>" |> new_nats chosen

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

let prompt_mercs x =
  Tty.writeln (sprintf "%d mercenaries available" x);
  Tty.prompt_yn "accept? y/n>"

let prompt_scouting () =
  Tty.prompt_yn "send scouts? y/n>"

let scouting_chosen s =
  if s then Tty.writeln "scouts sent"

let print_leader str x =
  Tty.writeln (sprintf "%s %s" str (leader2str x))

let print_support ls =
  let f = function
    | Some x -> res2str x
    | None -> "nothing"
  in
  ls
  |> List.map (fun (nat, res) -> (nation2str nat, f res))
  |> List.map (fun (nat, res) -> sprintf "%s sent %s" nat res)
  |> List.iter Tty.writeln

let print_status =
  let x = ref (S.get_res ()) in
  fun () ->
    let y = S.get_res () in
    if !x <> y then begin
      x := y;
      Tty.pairln "status" (res2str y)
    end

let r_out evt =
  let open Regular in
  match evt with
  | Attack ls ->
      Tty.pairln "attack" (enemies2str ls)
  | Blessing res ->
      Tty.pairln "blessing" (res2str res)
  | Casualty mp ->
      Tty.pairln "casualty" (manp2str mp)
  | LeaderDied x ->
      print_leader "leader died, was" x
  | LeaderLvup x ->
      print_leader "leader is now" x
  | LeaderNew x ->
      print_leader "new leader:" x
  | ScoutReport ls ->
      Tty.pairln "seen" (report2str ls)
  | ScoutSumReport x ->
      let (count, str) = sum2str x in
      sprintf "seen: about %d (%s)" count str
      |> Tty.writeln
  | Smite p ->
      Tty.pairln "smitten" (party2str p)
  | Starvation mp ->
      Tty.pairln "starvation" (manp2str mp)
  | Support ls ->
      print_support ls
  | Turn x ->
      Tty.lnwriteln ("turn " ^ string_of_int x)
  | Upkeep sup ->
      Tty.pairln "upkeep" (sup2str sup)
  | _ -> ()

let r_in evt =
  let open Regular in
  match evt with
  | Mercs (x, _) ->
      let ok = prompt_mercs x in
      Some (Mercs (x, ok))
  | Nations ls ->
      let ns = prompt_nations ls in
      nations_chosen ns;
      Some (Nations ns)
  | SendScouts _ ->
      let s = prompt_scouting () in
      scouting_chosen s;
      Some (SendScouts s)
  | _ -> None

let rec r_loop evt =
  if evt = Regular.End
  then Tty.fin ()
  else begin
    let e =
      match r_in evt with
      | Some x -> x
      | None -> r_out evt; evt
    in
    let x = R.next e in
    print_status ();
    Tty.flush ();
    r_loop x
  end

let rec i_loop evt =
  let next_with e =
    Tty.flush ();
    I.next e |> i_loop
  in
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
      print_leader "leader:" x;
      next ()
  | SendScouts _ ->
      let s = prompt_scouting () in
      scouting_chosen s;
      next_with (SendScouts s)
  | Starting res ->
      Tty.pairln "starting" (res2str res);
      next ()
  | Support ls ->
      print_support ls;
      next ()

let () =
  Random.self_init ();
  I.first () |> i_loop
