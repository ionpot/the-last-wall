let make () : Outcome.t =
  let a = Dice.deviate 10 5 in
  let b = Dice.deviate 5 5 in
  if Random.bool ()
  then Both (a, b)
  else Both (b, a)

let check () =
  if Dice.chance 0.8
  then make ()
  else None

let of_nations (ns: Nations.t) =
  let f n = (n, check ()) in
  List.map f ns.chosen
