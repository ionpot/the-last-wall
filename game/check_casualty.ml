let mitigate loss ldr =
  let def = Leader.defense_of ldr in
  truncate (loss -. loss *. def)

module Make (M : State.S) = struct
  let try_mitigate loss =
    match M.get_ldr () with
    | Some ldr -> mitigate loss ldr
    | None -> truncate loss

  let check enemies =
    let dmg = Enemy.damage enemies in
    let loss = try_mitigate dmg in
    if loss > 0
    then Some loss
    else None

  let is_victory = M.has_manp
end
