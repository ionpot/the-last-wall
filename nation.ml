type t =
  | Tulron
  | Sodistan
  | Hekatium
  | Numendor
  | Clan

let name_of = function
  | Tulron -> "Tulron"
  | Sodistan -> "Sodistan"
  | Hekatium -> "Hekatium"
  | Numendor -> "Numendor Council"
  | Clan -> "Undermountain Clan"

let ruler_of = function
  | Tulron -> "Melike Matis"
  | Sodistan -> "Sultan Azim III"
  | Hekatium -> "Emperor Levus IV"
  | Numendor -> "Archduke Norgalad"
  | Clan -> "Jarl Grorlon"

let to_list () =
  [ Tulron;
    Sodistan;
    Hekatium;
    Numendor;
    Clan;
  ]
