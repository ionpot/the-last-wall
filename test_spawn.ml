open Game
open Printf

let turn = ref 1

let str_of_sum x =
  let (count, str) = Convert.sum2str x in
  sprintf "%d (%s)" count str

let rec loop i =
  if i > 0
  then begin
    let e = Enemy.spawn !turn in
    printf "turn %d\nsumrep: %s\nreport: %s\nactual: %s\n"
      !turn
      (Scout.sum_report_of e |> str_of_sum)
      (Scout.report_of e |> Convert.report2str)
      (Convert.enemies2str e);
    incr turn;
    loop (pred i)
  end

let () =
  let count = try int_of_string Sys.argv.(1) with _ -> 5 in
  Random.self_init ();
  loop count
