module Make (M : State.S) = struct
  let blessing () =
    Deity.blessing_of (M.get_deity ())

  let smite enemies =
    if M.get_deity () = Deity.Lerota
    then Enemy.smite enemies
    else None
end
