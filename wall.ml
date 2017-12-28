type t =
  { age : int;
    supplies : int;
    manpower : int;
  }

let make () =
  { age = 0;
    supplies = Dice.between 90 180;
    manpower = Dice.between 10 30;
  }

let advance wall =
  { wall with age = succ wall.age }
