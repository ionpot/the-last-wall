let mitigate loss ldr =
  let x = Leader.mitigate loss ldr in
  Resource.(loss -- x)

module Make (M : State.S) = struct
  let try_mitigate loss =
    match M.get_ldr () with
    | Some ldr -> mitigate loss ldr
    | None -> loss

  let check enemies =
    let dmg = Enemy.damage enemies in
    let loss = try_mitigate dmg in
    if Resource.has_manp loss
    then Some loss
    else None

  let is_victory () =
    Resource.has_manp (M.get_res ())
end
