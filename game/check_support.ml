module Make (M : State.S) = struct
  let value =
    let ldr = M.Leader.return Leader.res_bonus_of in
    M.get_nats ()
    |> Nation.support_of_list
    |> Nation.apply_bonus ldr
end
