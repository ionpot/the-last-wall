module Make (M : State.S) = struct
  let smite enemies =
    if M.get_deity () = Deity.Lerota
    then Enemy.smite enemies
    else None
end
