type t = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

let ls = [Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Oct; Nov; Dec]

let empty = Jan

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

let pick () =
  Listx.pick_from ls
