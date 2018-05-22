module Initial = Game_initial
module Regular = Game_regular
module State = Game_state

module Make = State.Make
module Begin = Initial.Make
module Next = Regular.Make
