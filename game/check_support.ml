module Make (M : State.S) = struct
  let get_ldr_bonus () =
    match M.get_ldr () with
    | Some ldr -> Leader.res_bonus_of ldr
    | None -> Resource.empty

  let value =
    let ldr = get_ldr_bonus () in
    M.get_nats ()
    |> Nation.support_of_list
    |> Nation.apply_bonus ldr
end
