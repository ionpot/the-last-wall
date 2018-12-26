let dr_per_cav = 0.002
let dr_penalty = -0.05
let per_stable = 10
let ratio = 0.4
let strength = 2

let too_many cav men =
  float cav > (float men *. ratio)

module Check (M : State.S) = struct
  let cap = per_stable * M.bld_count Building.Stable
  let need = Number.sub cap (M.Cavalry.get ())
  let avlb = min need (M.get_supp ())
  let value = if avlb > 0 then Some avlb else None
end

module Dr (M : State.S) = struct
  let cav = M.Cavalry.get ()
  let men = M.get_manp ()
  let dr = float cav *. dr_per_cav
  let value = if too_many cav men then dr_penalty else dr
end
