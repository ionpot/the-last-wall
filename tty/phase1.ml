module Phase = Game.Phase1

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    let check f x = if x > 0 then f x else x in
    function
      | Build avlb ->
          let module Prompt = Prompt.Build(S) in
          S.Build.return Print.Build.all;
          Build (S.Build.return (Prompt.from avlb))
      | Deity _ -> Deity (Prompt.deity ())
      | Leader _ -> Leader (Prompt.leader ())
      | Nations chosen ->
          let module Prompt = Prompt.Nations(S) in
          Nations (Prompt.from chosen)
      | Scout _ -> Scout (Prompt.scout ())
      | Volunteers n -> Volunteers (check Prompt.volunteers n)

  let output =
    let open Phase.Output in
    function
      | BuildSupply s -> S.Supply.return (Print.Build.supply s)
      | Facilities ls -> Tty.ifpairln "facilities" (Convert.facs2str ls)
      | Starting (ldr, _, res) ->
          Tty.pairln "leader" (Convert.ldr2full ldr);
          Tty.pairln "starting" (Convert.res2str res)
      | Support s -> Print.support s
end

module After (S : Status.S) = struct
  let input =
    let open Phase.Input in
    function
      | Volunteers n -> if n > 0 then S.res ()
      | _ -> ()

  let output =
    let open Phase.Output in
    function
      | BuildSupply _ -> ()
      | Facilities ls -> if ls <> [] then S.res ()
      | Support _ -> S.res ()
      | Starting _ -> ()
end
