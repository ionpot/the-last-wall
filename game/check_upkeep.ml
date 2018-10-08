module Make (M : State.S) = struct
  module Scouting = Check_scouting.Make(M)

  let get () =
    let x = M.with_res Resource.manp2supp in
    let y = Scouting.get_cost () in
    Resource.(x ++ y)

  let get_starvation () =
    let res = M.with_res Resource.mis_supp in
    if res = Resource.empty
    then None
    else Some Resource.(supp2manp res)
end
