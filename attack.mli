type t =
  { enemy : Enemy.t;
    count : int;
  }

val build : Wall.t -> t list
val outcome_of : t -> Outcome.t
