module Phase = Game.Phase1

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    let check f x = if x > 0 then f x else x in
    function
      | Ballista (avlb, have) ->
          Ballista (check (Prompt.ballista have) avlb, have)
      | Build avlb ->
          S.Build.return Print.Build.all;
          Build (Prompt.Build.from avlb)
      | Deity _ -> Deity (Prompt.deity ())
      | Knight n -> Knight (check Prompt.knight n)
      | Leader _ -> Leader (Prompt.Leader.first ())
      | Nations chosen ->
          let module Prompt = Prompt.Nations(S) in
          Nations (Prompt.from chosen)
      | Scout _ -> Scout (Prompt.scout ())
      | Sodistan sup -> Sodistan (check Prompt.sodistan sup)
      | Trade _ -> Trade (Prompt.trade ())
      | Volunteers n -> Volunteers (check Prompt.volunteers n)

  let output =
    let open Phase.Output in
    function
      | BuildSupply s -> S.Supply.return (Print.Build.supply s)
      | Cavalry _ -> ()
      | Facilities x -> Print.facilities x
      | Starting s -> Print.starting s
      | Support s -> Print.support s
end

module After (S : Status.S) = struct
  let input =
    let open Phase.Input in
    function
      | Ballista (n, _) -> if n > 0 then S.res ()
      | Knight n -> if n > 0 then S.res ()
      | Leader _ -> S.leader ()
      | Sodistan n
      | Volunteers n -> if n > 0 then S.res ()
      | _ -> ()

  let output =
    let open Phase.Output in
    function
      | BuildSupply _ -> ()
      | Cavalry n -> if n > 0 then begin S.cavalry n; S.res () end
      | Facilities x -> S.facilities x
      | Support _ -> S.res ()
      | Starting _ -> ()
end
