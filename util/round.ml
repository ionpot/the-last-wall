let random x =
  let a = 0.1 *. float x in
  let f = if Random.bool () then floor else ceil in
  10 * truncate (f a)
