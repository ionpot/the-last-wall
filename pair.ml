let (+<) x (a, b) =
  (a + x, b)

let (+>) x (a, b) =
  (a, b + x)

let (++) (x, y) (a, b) =
  (a + x, b + y)
