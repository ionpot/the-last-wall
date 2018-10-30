module Make (M : State.S) = struct
  let scouting_cost () =
    if M.is_scouting ()
    then Resource.of_supp 10
    else Resource.empty

  let get () =
    let x = M.with_res Resource.manp2supp in
    let y = scouting_cost () in
    Resource.(x ++ y)

  let get_starvation () =
    let res = M.with_res Resource.mis_supp in
    if res = Resource.empty
    then None
    else Some Resource.(supp2manp res)
end
