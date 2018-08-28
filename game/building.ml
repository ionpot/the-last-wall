type t = Market | Tavern | Temple | Stable
type status =
  | Absent
  | Waiting of Resource.t
  | Built
  | Ready
type report = (t * status) list
type state = report

let initial = [Tavern, Ready]
let tlist = [Market; Tavern; Temple; Stable]

let cost_pair_of =
  let open Resource in
  function
  | Market -> Manpwr 48, Supply 53
  | Tavern -> Manpwr 0, Supply 0
  | Temple -> Manpwr 29, Supply 28
  | Stable -> Manpwr 49, Supply 54

let is_t t (x, _) = x = t

let in_state state t =
  List.exists (is_t t) state

let deduce_manp res r =
  let new_res, new_r = Resource.deduce_manp res r in
  if new_r = Resource.empty
  then new_res, Built
  else new_res, Waiting new_r

let advance res = function
  | Absent -> res, Absent
  | Waiting r ->
      if Resource.has_supp r
      then res, Waiting r
      else deduce_manp res r
  | Built
  | Ready -> res, Ready

let rec apply_manp res = function
  | [] -> []
  | (t, s) :: rest ->
      let new_res, new_s = advance res s in
      (t, new_s) :: apply_manp new_res rest

let apply_supp res = function
  | Waiting r ->
      let new_res, new_r = Resource.deduce_supp res r in
      new_res, Waiting new_r
  | x -> res, x

let draw_supp res state =
  let f (r, new_state) (t, status) =
    let rem, new_status = apply_supp r status in
    rem, (t, new_status) :: new_state
  in
  let (r, s) = List.fold_left f (res, []) state in
  r, List.rev s

let cost_of t =
  let a, b = cost_pair_of t in
  Resource.(empty <+ a <+ b)

let add ls state =
  List.filter (fun x -> not @@ in_state state x) ls
  |> List.map (fun t -> t, Waiting (cost_of t))
  |> List.append state

let build ls res state =
  add ls state |> draw_supp res

let is_ready t state =
  List.exists ((=) (t, Ready)) state

let status_of t state =
  match List.filter (is_t t) state with
  | [] -> Absent
  | (_, x) :: _ -> x

let report_of state =
  List.map (fun x -> x, status_of x state) tlist
