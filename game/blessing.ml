module Check (M : State.S) = struct
  let get_res =
    if M.bld_ready Building.Temple
    then Deity.boosted_of
    else Deity.blessing_of

  let value =
    let res = M.with_deity get_res in
    if res = Resource.empty
    then None
    else Some res
end
