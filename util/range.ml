type 'a t = 'a * 'a

module Int = struct
  type nonrec t = int t

  let add i (a, b) =
    a + i, b + i
end

