module With (S : State.S) = struct
  let dervish_range () =
    if S.Build.check Build.(ready Guesthouse)
    then 3, 12 else 2, 8

  module Missing = struct
    let stable () =
      Number.sub
        (S.Build.return Build.stable_cap)
        (S.Units.return Units.count_cavalry)

    let temple () =
      Number.sub
        (S.Build.return Build.temple_cap)
        (S.Units.return Units.count_holy)
  end

  let supply_limit kind cap =
    let cost = Units.Cost.supply kind in
    let supp = S.Supply.get () in
    min cap (Number.div supp cost)

  let exclude () =
    let n = S.Dervish.get () in
    S.Units.return Units.(sub n Dervish)

  let affordable kind cap =
    exclude () |> Units.affordable kind cap |> supply_limit kind

  let promotable kind =
    exclude () |> Units.promotable kind |> supply_limit kind

  let sub_cost kind n =
    S.Supply.sub (Units.Cost.supply kind * n);
    S.Units.map Units.(Cost.from n kind |> reduce)

  let promote kind n =
    sub_cost kind n;
    S.Units.map Units.(add n kind)
end
