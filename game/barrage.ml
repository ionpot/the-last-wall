let dr_penalty = 0.05

module Check (M : State.S) = struct
  let value =
    let x = M.get_manp () / 10 in
    M.with_enemies Enemy.(find x Orc)
end
