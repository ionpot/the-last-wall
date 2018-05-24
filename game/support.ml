open Defs

type resource = Resource.t
type t = (nation * resource option)

let of_nation n =
  (n, Outcome.support ())

let of_nats ns =
  List.map of_nation ns

let total_of ns =
  let open Resource in
  let f total (_, maybe) =
    match maybe with
    | Some res -> total ++ res
    | None -> total
  in
  List.fold_left f (make Empty) ns
