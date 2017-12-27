type t =
  | Tulron
  | Sodistan
  | Hekatium
  | Numendor
  | Clan

type stats =
  { name : string;
    ruler : string;
  }

let stats_of = function
  | Tulron ->
      { name = "Tulron";
        ruler = "Melike Matis";
      }
  | Sodistan ->
      { name = "Sodistan";
        ruler = "Sultan Azim III";
      }
  | Hekatium ->
      { name = "Hekatium";
        ruler = "Emperor Levus IV";
      }
  | Numendor ->
      { name = "Numendor Council";
        ruler = "Archduke Norgalad";
      }
  | Clan ->
      { name = "Undermountain Clan";
        ruler = "Jarl Grorlon";
      }

let to_list () =
  [ Tulron;
    Sodistan;
    Hekatium;
    Numendor;
    Clan;
  ]
