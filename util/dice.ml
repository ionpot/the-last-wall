let roll i =
  Random.int i + 1

let chance fl =
  let x = Random.float 1.0 in
  x < fl

let between x y =
  let d = y - x in
  x + Random.int (d + 1)

let deviate x y =
  let a = x - y in
  let b = x + y in
  between a b
