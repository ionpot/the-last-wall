module With (S : State.S) = struct
  let dervish_range () =
    if S.Build.check Build.(ready Guesthouse)
    then 3, 12 else 2, 8

  let ballista_cap () =
    if S.Build.check Build.(ready Engrs)
    then 5 else 0

  let stable_cap () =
    Number.sub
    (S.Build.return Build.stable_cap)
    (S.Units.return Units.(count Cavalry))

  let temple_cap () =
    Number.sub
    (S.Build.return Build.temple_cap)
    (S.Units.return Units.count_holy)

  let supply_limit kind cap =
    let cost = Units.supply_cost_of kind in
    let supp = S.Supply.get () in
    min cap (Number.div supp cost)

  let exclude kind n =
    Units.sub n kind
    |> S.Units.return

  let units_for kind =
    if kind = Units.Dervish
    then S.Dervish.return (exclude kind)
    else S.Units.get ()

  let affordable kind cap =
    min (supply_limit kind cap)
    (units_for kind |> Units.affordable kind cap)

  let sub_cost kind n =
    S.Supply.sub (Units.supply_cost_of kind * n);
    S.Units.map Units.(make_cost n kind |> reduce)

  let promote kind n =
    sub_cost kind n;
    S.Units.map Units.(add n kind)
end
