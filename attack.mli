type t =
  { enemy : Enemy.t;
    count : int;
    outcome : Outcome.t;
  }

val build : Game.t -> t list
