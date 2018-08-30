open Game
open Printf

type t =
  { skel : int;
    orc : int;
    demon : int;
    dmr : float
  }

let ws = Str.regexp "[ \t]"

let init =
  { skel = 0;
    orc = 0;
    demon = 0;
    dmr = 0.0
  }

let split line =
  Str.split ws line
  |> List.filter ((<>) "")

let prompt () =
  print_string "> ";
  read_line ()
  |> String.trim

let enemy2str =
  let open Enemy in
  function
    | Skeleton -> "skeleton"
    | Orc -> "orc"
    | Demon -> "demon"

let pair2str (n, k) =
  sprintf "%d %s(%.1f)" n (enemy2str k) (Enemy.power_of k)

let pairs2str pairs =
  List.map pair2str pairs
  |> String.concat ", "

let def2str def dmr =
  sprintf "%d (%.1f%%)" def (dmr *. 100.0)

let calc2str dmg def dmr =
  sprintf "%d - %s = %d"
    dmg (def2str def dmr) (dmg - def)

let pair2party (n, k) =
  Enemy.make n k

let to_pairs t =
  Enemy.([t.skel, Skeleton; t.orc, Orc; t.demon, Demon])

let pairs2dmg pairs =
  pairs
  |> List.map pair2party
  |> Enemy.damage
  |> Resource.manp_of

let add_dmr x t = { t with dmr = t.dmr +. x }
let sub_dmr x t = { t with dmr = t.dmr -. x }
let set_dmr x t = { t with dmr = x }
let add_skel x t = { t with skel = t.skel + x }
let sub_skel x t = { t with skel = t.skel - x }
let set_skel x t = { t with skel = x }
let add_orc x t = { t with orc = t.orc + x }
let sub_orc x t = { t with orc = t.orc - x }
let set_orc x t = { t with orc = x }
let add_demon x t = { t with demon = t.demon + x }
let sub_demon x t = { t with demon = t.demon - x }
let set_demon x t = { t with demon = x }

let with_int f str state =
  try f (int_of_string str) state with
  | _ -> state

let with_float f str state =
  try f (float_of_string str /. 100.0) state with
  | _ -> state

let check_apply chars str t =
  match chars with
  | 'r', '+' -> with_float add_dmr str t
  | 's', '+' -> with_int add_skel str t
  | 'o', '+' -> with_int add_orc str t
  | 'd', '+' -> with_int add_demon str t
  | 'r', '-' -> with_float sub_dmr str t
  | 's', '-' -> with_int sub_skel str t
  | 'o', '-' -> with_int sub_orc str t
  | 'd', '-' -> with_int sub_demon str t
  | 'r', _ -> with_float set_dmr str t
  | 's', _ -> with_int set_skel str t
  | 'o', _ -> with_int set_orc str t
  | 'd', _ -> with_int set_demon str t
  | _ -> t

let offset_of = function
  | '+' | '-' -> 2
  | _ -> 1

let apply_command state str =
  let chars = (str.[0], str.[1]) in
  let offset = offset_of str.[1] in
  let value = Str.string_after str offset in
  check_apply chars value state

let check_command state str =
  if str = "c" then init
  else apply_command state str

let parse_line line state =
  split line
  |> List.fold_left check_command state

let mitigate dmg dmr =
  truncate (float dmg *. dmr)

let print_pairs pairs =
  print_endline (pairs2str pairs)

let print_calc pairs t =
  let dmg = pairs2dmg pairs in
  let def = mitigate dmg t.dmr in
  print_endline (calc2str dmg def t.dmr)

let print_outcome state =
  let pairs = to_pairs state in
  print_pairs pairs;
  print_calc pairs state

let rec loop state =
  match prompt () with
  | "q" -> ()
  | line ->
      let new_state = parse_line line state in
      print_outcome new_state;
      loop new_state

let () =
  loop init
