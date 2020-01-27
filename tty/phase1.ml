module Phase = Game.Phase1
module Units = Game.Units

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    let check f x = if x > 0 then f x else x in
    let promote k = check (Prompt.promote k) in
    function
      | Ballista avlb -> Ballista (check (Prompt.siege Units.Ballista) avlb)
      | Build avlb ->
          let nat = S.Nation.get () in
          S.Build.return (Print.Build.all nat);
          Build (Prompt.Build.from nat avlb)
      | Deity _ -> Deity (Prompt.deity ())
      | Knight n -> Knight (promote Units.Knight n)
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
      | BuildSupply s -> Print.Build.supply s
      | Cavalry _ -> ()
      | Facilities x ->
          let nat = S.Nation.get () in
          Print.facilities nat x
      | Starting s ->
          let nat = S.Nation.get () in
          Print.starting nat s
      | Support s -> Print.support s
end

module After (S : Status.S) = struct
  let promote k n =
    if n > 0 then begin S.promote k; S.res () end

  let input =
    let open Phase.Input in
    function
      | Ballista n -> if n > 0 then S.res ()
      | Knight n -> promote Units.Knight n
      | Leader _ -> S.leader ()
      | Sodistan n
      | Volunteers n -> if n > 0 then S.res ()
      | _ -> ()

  let output =
    let open Phase.Output in
    function
      | BuildSupply s -> if s > 0 then S.res ()
      | Cavalry n -> if n > 0 then begin S.cavalry n; S.res () end
      | Facilities x -> S.facilities x
      | Support _ -> S.res ()
      | Starting _ -> ()
end
