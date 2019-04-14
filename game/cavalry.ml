let per_stable = 10

module Count (S : State.S) = struct
  let count = S.Build.return Build.(count Stable)
  let cap = count * per_stable
end

module Check (S : State.S) = struct
  module C = Count(S)
  let value = C.cap > 0
    && S.Cavalry.get () < C.cap
    && S.Men.ptv ()
    && S.Supply.ptv ()
end

module Make (S : State.S) = struct
  module C = Count(S)
  let need = C.cap - S.Cavalry.get ()
  let value =
    S.Men.get ()
    |> min (S.Supply.get ())
    |> min need
end
