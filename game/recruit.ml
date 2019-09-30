module With (S : State.S) = struct
  module Base = Units.Base

  let dervish_range () =
    if S.Build.check Build.(is_ready Guesthouse)
    then 3, 12 else 2, 8

  module Missing = struct
    let arena () =
      Units.(count Berserker)
      |> S.Units.return
      |> Number.sub (S.Build.return Build.arena_cap)

    let stable () =
      Units.(filter_count Attr.is_cavalry)
      |> S.Units.return
      |> Number.sub (S.Build.return Build.stable_cap)

    let temple () =
      Units.(filter_count Attr.is_holy)
      |> S.Units.return
      |> Number.sub (S.Build.return Build.temple_cap)
  end

  let supply_limit kind cap =
    let cost = Base.supply_cost kind in
    if cost > 0 then
      let supp = S.Supply.get () in
      min cap (supp / cost)
    else cap

  let exclude () =
    let n = S.Dervish.get () in
    S.Units.return Units.(sub n Dervish)

  let affordable kind cap =
    exclude () |> Units.affordable kind cap |> supply_limit kind

  let promotable kind =
    exclude () |> Units.promotable kind |> supply_limit kind

  let sub_cost kind n =
    S.Supply.sub (Base.supply_cost kind * n);
    S.Units.map Units.(cost n kind |> reduce)

  let promote kind n =
    sub_cost kind n;
    S.Units.map Units.(add n kind)
end
