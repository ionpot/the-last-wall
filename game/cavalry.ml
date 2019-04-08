let dr_per_cav = 0.002
let dr_penalty = -0.05
let per_stable = 10
let ratio = 0.4
let strength = 2.

let too_many cav men =
  float cav > (float men *. ratio)

let dr men cav =
  if too_many cav men
  then dr_penalty
  else float cav *. dr_per_cav

module Check (S : State.S) = struct
  let cap = per_stable * S.bld_count Building.Stable
  let value = cap > 0
    && S.Cavalry.get () < cap
    && S.Men.ptv ()
    && S.Supply.ptv ()
end

module Make (S : State.S) = struct
  let cap = per_stable * S.bld_count Building.Stable
  let need = cap - S.Cavalry.get ()
  let value =
    S.Men.get ()
    |> min (S.Supply.get ())
    |> min need
end
