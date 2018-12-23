type event =
  | Died of Leader.t
  | LvUp of Leader.t

let lvlup ldr =
  if Leader.can_lvup ldr
  then Some (LvUp (Leader.lvup ldr))
  else None

let survives ldr =
  if Leader.lives ()
  then lvlup ldr
  else Some (Died ldr)

module Make (M : State.S) = struct
  let check () =
    match M.get_ldr () with
    | Some ldr -> survives ldr
    | None -> None
end
