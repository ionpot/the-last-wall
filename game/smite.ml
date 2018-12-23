module Check (M : State.S) = struct
  let boost =
    if M.bld_ready Building.Temple
    then 10
    else 0

  let find () =
    let x = Dice.between 10 30 + boost in
    M.with_enemies Enemy.(find x Skeleton)

  let value =
    if M.get_deity () = Deity.Lerota
    then find ()
    else None
end
