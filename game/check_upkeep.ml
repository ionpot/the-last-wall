module Make (M : State.S) = struct
  module Scouting = Check_scouting.Make(M)

  let get () =
    let res = M.get_res () in
    let x = Resource.manp2supp res in
    let y = Scouting.get_cost () in
    Resource.(x ++ y)

  let get_starvation () =
    let res = Resource.mis_supp (M.get_res ()) in
    if res = Resource.empty
    then None
    else Some Resource.(supp2manp res)
end
