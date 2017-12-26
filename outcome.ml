type t =
  { supply : int;
    manpower : int;
  }

let make supply manpower =
  { supply; manpower }

let apply t w =
  { w with
    supplies = w.supplies + t.supplies;
    manpower = w.manpower + t.manpower;
  }
