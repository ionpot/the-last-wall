type t =
  | Skeleton
  | Orc
  | Demon

type stats =
  { power : int;
    chance : float;
    modifier : float;
  }

let stats_of = function
  | Skeleton ->
      { power = 1;
        chance = 0.8;
        modifier = 1.0;
      }
  | Orc ->
      { power = 3;
        chance = 0.6;
        modifier = 0.6;
      }
  | Demon ->
      { power = 5;
        chance = 0.4;
        modifier = 0.3;
      }

let to_list () =
  [ Skeleton; Orc; Demon ]

let random () =
  let ls = to_list () in
  let len = List.length ls in
  let i = Random.int len in
  List.nth ls i
