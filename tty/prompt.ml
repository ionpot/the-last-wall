open Convert
open Game
open Printf

let ch_cmp (a, _) (b, _) = Char.compare a b

let barrage () =
  Tty.prompt_yn "arrow barrage? y/n>"

let bld_chars =
  List.map bld2char Building.tlist

let bld_list =
  Building.tlist
  |> List.map (fun b -> bld2char b, b)
  |> List.sort ch_cmp

let find_bld ch =
  List.find (fun (c, _) -> c = ch) bld_list
  |> snd

let pick_blds bs str =
  chars_of str
  |> List.filter (fun ch -> List.mem ch bld_chars)
  |> List.map find_bld
  |> Listx.discard (fun b -> Buildings.is_ignored b bs)
  |> Listx.build (fun ls b ->
      if not (Building.multiple b) && List.mem b ls
      then ls else b :: ls)
  |> List.rev

let build bs =
  Tty.writeln "buildings:";
  let to_str (ch, b) =
    let s =
      if Buildings.is_ignored b bs then "  " else sprintf "%c)" ch
    in
    sprintf "  %s %s %s" s (bld2str b) (bstat2str b bs)
  in
  List.map to_str bld_list
  |> List.iter Tty.writeln;
  Tty.prompt "build?>" |> pick_blds bs

let deity () =
  Tty.write "deities:";
  Deity.t_list
  |> List.map (fun d -> (deity2char d, deity2str d))
  |> List.sort ch_cmp
  |> List.map (fun (c, s) -> sprintf " %c) %s" c s)
  |> List.iter Tty.write;
  Tty.lnprompt_char "choose>" |> char2deity

let leader () =
  Tty.write "leaders:";
  Leader.ltypes
  |> List.map (fun d -> (ltype2char d, ltype2str d))
  |> List.sort ch_cmp
  |> List.map (fun (c, s) -> sprintf " %c) %s" c s)
  |> List.iter Tty.write;
  Tty.lnprompt_char "choose>" |> char2ltype

let nat_list =
  Nation.t_list
  |> List.map (fun n -> nation2char n, n)
  |> List.sort ch_cmp

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

let nations chosen =
  Tty.write "nations:";
  nat_list
  |> List.map (fun (c, n) -> c, nat_str n chosen)
  |> List.map (fun (c, s) -> sprintf " %c) %s" c s)
  |> List.iter Tty.write;
  Tty.lnprompt "choose>" |> new_nats chosen

let mercs x =
  Tty.writeln (sprintf "%d mercenaries available" x);
  Tty.prompt_yn "accept? y/n>"

let scouting () =
  Tty.prompt_yn "send scouts? y/n>"
