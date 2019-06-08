module From (S : State.S) = struct
  let ls =
    [ S.Build.check Build.(ready Engrs),
      Build.(ToAll, Resource.Bonus.(Sub (Sup 0.1)));
      S.Deity.is Deity.Elanis,
      Build.(To Stable, Resource.Bonus.(Sub (Both 0.1)))
    ]
  let value = List.filter fst ls |> List.map snd
end
