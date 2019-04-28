module Phase = Game.Phase1

module Make (S : Game.State.S) = struct
  let input =
    let open Phase.Input in
    function
      | Build avlb -> Build (S.Build.return (Prompt.build avlb))
      | Deity _ -> Deity (Prompt.deity ())
      | Leader _ -> Leader (Prompt.leader ())
      | Nations chosen -> Nations (Prompt.nations chosen)
      | Scout _ -> Scout (Prompt.scout ())

  let output _ = ()
end

(*
let phase1 evt update =
  let open Game.Phase1 in
  match evt with
  | Build _ ->
      let bs = S.with_bld Prompt.build in
      Print.bld_chosen bs;
      update (Build bs)
  | BuildSupply x -> Print.bld_supp x
  | End -> ()
  | Nations x ->
      let ns = Prompt.nations x in
      Print.nations_chosen ns;
      update (Nations ns)
  | SendScouts _ ->
      let s = Prompt.scouting () in
      Print.scouting_chosen s;
      update (SendScouts s)
  | Starting res ->
      Tty.pairln "starting" (res2str res)
  | Support ls ->
      Print.support ls
      *)
