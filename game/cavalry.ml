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

let how_many (mn, mx) =
  Dice.between mn mx

let range_of dmg men cav =
  let f x = min (x / strength) cav in
  let mn = Number.sub dmg men in
  f mn, f dmg

let calc dmg cav men =
  let roll = Random.float (float (cav + men)) in
  if roll > float men
  then how_many (range_of dmg men cav)
  else 0

module Casualty (M : State.S) = struct
  let cav = M.Cavalry.get ()
  let men = M.get_manp ()
  let check dmg = if cav = 0 then 0 else calc dmg cav men
end
