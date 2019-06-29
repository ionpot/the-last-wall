module With (S : State.S) = struct
  let cap_rem () =
    let cap = S.Build.return Build.temple_cap in
    let count = S.Units.return Units.count_holy in
    max 0 (cap - count)

  let cap_for kind =
    let cap = cap_rem () in
    let cost = Units.supply_cost_of kind in
    let supp = S.Supply.get () in
    if cost > 0 then min cap (supp / cost) else cap

  let dervish_range () =
    if S.Build.check Build.(ready Guesthouse)
    then 3, 12 else 2, 8

  let promotable () =
    max 0 (S.Units.return Units.(count Dervish) - S.Dervish.get ())

  let buy kind n =
    S.Supply.sub (Units.supply_cost_of kind * n);
    S.Units.map Units.(add n kind)

  let promote kind n =
    buy kind n;
    S.Units.map Units.(sub n Dervish)
end
