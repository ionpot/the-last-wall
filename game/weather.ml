type degree = Light | Heavy
type t =
  | Breeze
  | Clear
  | Cloudy
  | Fog
  | Heat
  | Wind
  | Rain of degree
  | Snow of degree

let empty = Clear

let is_bad = function
  | Fog | Wind | Rain Heavy | Snow Heavy -> true
  | _ -> false

let possible =
  let open Month in
  function
    | Mar | Apr | May -> [Breeze; Clear; Cloudy; Fog; Rain Light; Rain Heavy]
    | Jun | Jul | Aug -> [Breeze; Clear; Clear; Cloudy; Heat; Rain Light]
    | Sep | Oct | Nov -> [Cloudy; Fog; Rain Light; Rain Heavy; Wind]
    | Dec | Jan | Feb -> [Cloudy; Fog; Rain Heavy; Snow Light; Snow Heavy]

module Roll (Dice : Dice.S) = struct
  let random month = Dice.pick (possible month)
end
