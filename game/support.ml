open Defs

type resource = Resource.t
type t = (nation * resource option)

let total_of ns =
  let open Resource in
  let f total (_, maybe) =
    match maybe with
    | Some res -> total ++ res
    | None -> total
  in
  List.fold_left f (make Empty) ns

let to_outcome () =
  let open Resource in
  let f () =
    let a = Dice.deviate 10 5 in
    let b = Dice.deviate 5 5 in
    let (m, s) =
      if Random.bool ()
      then (a, b)
      else (b, a)
    in
    make (Manpwr m) <+ Supply s
  in
  if Dice.chance 0.8
  then Some (f ())
  else None

let of_nation n =
  (n, to_outcome ())

let of_nats ns =
  List.map of_nation ns
