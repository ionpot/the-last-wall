module Count (S : State.S) = struct
  let cavs = S.Units.return Units.(count Cavalry)
  let men = S.Units.return Units.(count Men)
  let cap = S.Build.return Build.stable_cap
end

module Check (S : State.S) = struct
  module C = Count(S)
  let value = C.cap > 0
    && C.cavs < C.cap
    && C.men > 0
    && S.Supply.ptv ()
end

module Make (S : State.S) = struct
  module C = Count(S)
  let need = C.cap - C.cavs
  let sup = S.Supply.get ()
  let value = min need (min C.men sup)
end
