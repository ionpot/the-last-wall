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

let (~+) (x, y) (a, b) =
  (x - a, y - b)
