let dr_penalty = 0.05

module Check (M : State.S) = struct
  let barrage () =
    let x = M.get_manp () / 20 in
    M.with_enemies Enemy.(find x Orc)

  let value =
    if M.Barraging.get ()
    then barrage ()
    else None
end
