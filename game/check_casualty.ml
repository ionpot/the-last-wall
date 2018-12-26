open Defs

type t =
  | Loss of (manpower * manpower)
  | Fort of (manpower * manpower)
  | Ok

let cav_str = Cavalry.strength
let fort_cap = 20

let sub a b = a -. a *. b

let mp_of cav = cav * cav_str

let rec rand cap (m, men) (c, cav) =
  if cap = 0 then m, c
  else
    let ms = min cap men in
    let cs = min (cap / cav_str) cav in
    let x =
      if ms = 0 then cs
      else if cs = 0 then ms
      else Dice.roll (ms + cs)
    in
    if x > ms
    then
      let y = x - ms in
      rand (cap - y * cav_str) (m, ms) (c + y, cs - y)
    else rand (cap - x) (m + x, ms - x) (c, cs)

let pick cap men cav =
  if cap < men + mp_of cav
  then rand cap (0, men) (0, cav)
  else men, cav

module Make (M : State.S) = struct
  module Cav_dr = Cavalry.Dr(M)

  let is_defeat loss =
    let cav_mp = M.Cavalry.return mp_of in
    loss > M.get_manp () + cav_mp

  let pick_for cap =
    pick cap (M.get_manp ()) (M.Cavalry.get ())

  let mitigate loss ldr =
    let def = Leader.defense_of ldr in
    let brg =
      if M.Barraging.get ()
      then Barrage.dr_penalty
      else 0.
    in
    truncate (sub loss (def -. brg))

  let try_mitigate loss =
    let x = sub loss Cav_dr.value in
    match M.get_ldr () with
    | Some ldr -> mitigate x ldr
    | None -> truncate x

  let check_fort loss =
    if is_defeat loss && M.bld_ready Building.Fort
    then Fort (pick_for fort_cap)
    else Loss (pick_for loss)

  let check enemies =
    let dmg = Enemy.damage enemies in
    let loss = try_mitigate dmg in
    if loss > 0
    then check_fort loss
    else Ok

  let is_victory () =
    M.has_manp () || M.Cavalry.ptv ()
end
