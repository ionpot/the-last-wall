let sub a b = a -. a *. b

let mitigate loss ldr =
  let def = Leader.defense_of ldr in
  truncate (sub loss def)

module Make (M : State.S) = struct
  module Cav_dr = Cavalry.Dr(M)

  let cas_of loss =
    let module Cav = Cavalry.Casualty(M) in
    let cav = Cav.check loss in
    loss - cav, cav

  let try_mitigate loss =
    let x = sub loss Cav_dr.value in
    match M.get_ldr () with
    | Some ldr -> mitigate x ldr
    | None -> truncate x

  let check enemies =
    let dmg = Enemy.damage enemies in
    let loss = try_mitigate dmg in
    if loss > 0
    then Some (cas_of loss)
    else None

  let is_victory () =
    M.has_manp () || M.Cavalry.ptv ()
end
