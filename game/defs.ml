type count = int
type manpower = int
type power = float
type scouting = bool
type supply = int
type turn = int

let to_power : count -> power -> power =
  (fun count power -> float count *. power)
