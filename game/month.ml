type t = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

let ls = [Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Oct; Nov; Dec]

let empty = Jan

let is_winter = function
  | Jan | Feb | Dec -> true
  | _ -> false

let next = function
  | Jan -> Feb
  | Feb -> Mar
  | Mar -> Apr
  | Apr -> May
  | May -> Jun
  | Jun -> Jul
  | Jul -> Aug
  | Aug -> Sep
  | Sep -> Oct
  | Oct -> Nov
  | Nov -> Dec
  | Dec -> Jan

module Roll (Dice : Dice.S) = struct
  let random () = Dice.pick ls
end
