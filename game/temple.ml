module With (S : State.S) = struct
  let cap_rem () =
    let cap = S.Build.return Build.temple_cap in
    let count = S.Units.return Units.in_temple in
    max 0 (cap - count)

  let cap_for kind =
    let cap = cap_rem () in
    let cost = Units.cost_of kind in
    let supp = S.Supply.get () in
    if cost > 0 then min cap (supp / cost) else cap

  let buy kind n =
    S.Supply.sub (Units.cost_of kind * n);
    S.Units.map Units.(add n kind)
end
