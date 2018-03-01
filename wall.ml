type t =
  { supplies : int;
    manpower : int;
    leader : bool;
  }

let make () =
  { supplies = 0;
    manpower = 0;
    leader = true;
  }

let is_destroyed wall =
  wall.manpower <= 0
