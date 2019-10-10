type 'a t = 'a * 'a

module Int = struct
  type nonrec t = int t

  let add i (a, b) =
    a + i, b + i

  let combine (a, b) (x, y) =
    a + x, b + y

  let combine_if cond a b =
    if cond then combine a b else b

  let times i (a, b) =
    a * i, b * i
end

