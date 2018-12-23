module Check (M : State.S) = struct
  let value =
    if M.bld_ready Building.Market
    then Some (Dice.between 15 45)
    else None
end
