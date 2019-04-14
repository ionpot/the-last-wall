type degree = Light | Heavy
type t =
  | Sunny
  | Clear
  | Cloudy
  | Wind
  | Rain of degree
  | Snow of degree

let empty = Sunny

let is_bad = function
  | Wind | Rain Heavy | Snow Heavy -> true
  | _ -> false

let possible =
  let open Month in
  function
    | Mar | Apr | May -> [Clear; Cloudy; Rain Light; Rain Heavy]
    | Jun | Jul | Aug -> [Clear; Cloudy; Rain Light; Sunny]
    | Sep | Oct | Nov -> [Cloudy; Rain Light; Rain Heavy; Wind]
    | Dec | Jan | Feb -> [Cloudy; Rain Heavy; Snow Light; Snow Heavy]

let pick month =
  Listx.pick_from (possible month)
