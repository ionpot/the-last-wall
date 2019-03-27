let dr_per_cav = 0.002
let dr_penalty = -0.05
let per_stable = 10
let ratio = 0.4
let strength = 2

let too_many cav men =
  float cav > (float men *. ratio)

module Check (M : State.S) = struct
  let cap = per_stable * M.bld_count Building.Stable
  let value = cap > 0
    && M.Cavalry.get () < cap
    && M.has_manp ()
    && M.has_supp ()
end

module Make (M : State.S) = struct
  let cap = per_stable * M.bld_count Building.Stable
  let need = cap - M.Cavalry.get ()
  let value = Listx.min_of [M.get_manp (); M.get_supp (); need]
end

module Dr (M : State.S) = struct
  let cav = M.Cavalry.get ()
  let men = M.get_manp ()
  let dr = float cav *. dr_per_cav
  let value = if too_many cav men then dr_penalty else dr
end
