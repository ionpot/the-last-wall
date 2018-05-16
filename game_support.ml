open Game_defs

module O = Game_outcomes
module R = Game_resource

type resource = R.t
type t = (nation * resource option)

let of_nation n =
  (n, O.support ())

let of_list ns =
  List.map of_nation ns

let total_of ns =
  let open R in
  let f total (_, maybe) =
    match maybe with
    | Some res -> total ++ res
    | None -> total
  in
  List.fold_left f (make Empty) ns
