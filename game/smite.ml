module Check (M : State.S) = struct
  let boost =
    if M.bld_ready Building.Temple
    then 10
    else 0

  let find enemies =
    let x = Dice.between 10 30 + boost in
    Enemy.(find x Skeleton enemies)

  let attacking enemies =
    if M.get_deity () = Deity.Lerota
    then find enemies
    else None
end
