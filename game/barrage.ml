type reason = Archers | Leader | Weather
type status = Available | Disabled of reason

let base_coefficient = 0.05

type t =
  { choice : bool
  ; status : status
  ; trained : bool
  }

let empty =
  { choice = false
  ; status = Available
  ; trained = false
  }

let is_available t = t.status = Available
let is_chosen t = t.choice
let is_trained t = t.trained

let can_hit_run t =
  is_available t && not (is_chosen t)

let coefficient bonus t =
  let add_if b = Float.add_if (Bonus.has b bonus) in
  (if is_trained t then 0.1 else base_coefficient)
  |> add_if Bonus.ClearSky 0.02
  |> add_if Bonus.NumendorTrade 0.02

let set_choice choice t =
  { t with choice }

let set_status status t =
  { t with status }

let set_trained trained t =
  { t with trained }
