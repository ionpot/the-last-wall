open Game_defs

module R = Resource

type t =
  | Manp of manpower
  | Supp of supply
  | Both of (manpower * supply)
  | None

let res_of o =
  let res = R.make () in
  match o with
  | Manp x -> R.add_m x res
  | Supp x -> R.add_s x res
  | Both (m, s) ->
      res |> R.add_m m |> R.add_s s
  | None -> res

let blessing d =
  let r n = Dice.deviate n 5 in
  let o = match d with
    | Elanis -> Manp (r 15)
    | Sitera -> Supp (r 15)
    | Sekrefir -> Both ((r 5), (r 5))
    | None -> None
  in
  res_of o

let starting () =
  let man = Dice.between 10 30 in
  let sup = Dice.between 90 180 in
  res_of (Both (man, sup))

let support () =
  let make () =
    let a = Dice.deviate 10 5 in
    let b = Dice.deviate 5 5 in
    let o =
      if Random.bool ()
      then Both (a, b)
      else Both (b, a)
    in
    res_of o
  in
  if Dice.chance 0.8
  then Some (make ())
  else None
