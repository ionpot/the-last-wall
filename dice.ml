let roll i = Random.int i + 1

let chance fl =
  let x = Random.float 1.0
  in x < fl

let between x y =
  let d = y - x
  in x + Random.int d

let deviate x y =
  x + Random.int (y * 2) - y
