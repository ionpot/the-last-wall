let (+<) x (a, b) =
  (a + x, b)

let (+>) x (a, b) =
  (a, b + x)

let (~<) x (a, b) =
  (a - x, b)

let (~>) x (a, b) =
  (a, b - x)

let (++) (x, y) (a, b) =
  (a + x, b + y)

let (--) (x, y) (a, b) =
  (x - a, y - b)

let (<+>) x y =
  x ++ (y, y)

let snd_set x (a, _) = a, x

let fst_to f (a, _) = f a
let snd_to f (_, a) = f a

let eq_map f (x, y) (a, b) =
  if x = a then x, f y b else a, b

let map (f, g) (a, b) = (f a, g b)
