type degree = Light | Heavy
type t =
  | Breeze
  | Clear
  | Cloudy
  | Wind
  | Rain of degree
  | Snow of degree

let empty = Clear

let is_bad = function
  | Wind | Rain Heavy | Snow Heavy -> true
  | _ -> false

let possible =
  let open Month in
  function
    | Mar | Apr | May -> [Breeze; Clear; Cloudy; Rain Light; Rain Heavy]
    | Jun | Jul | Aug -> [Breeze; Clear; Clear; Cloudy; Rain Light]
    | Sep | Oct | Nov -> [Cloudy; Rain Light; Rain Heavy; Wind]
    | Dec | Jan | Feb -> [Cloudy; Rain Heavy; Snow Light; Snow Heavy]

module Roll (Dice : Dice.S) = struct
  let random month = Dice.pick (possible month)
end
