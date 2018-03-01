type t =
  | None
  | Supply of int
  | Manpower of int
  | Both of int * int
  | Leader of bool

let rec apply_to (wall: Wall.t) = function
  | None -> wall
  | Supply i ->
      { wall with
        supplies = wall.supplies + i;
      }
  | Manpower i ->
      { wall with
        manpower = wall.manpower + i;
      }
  | Both (s, m) ->
      apply_to (apply_to wall (Supply s)) (Manpower m)
  | Leader leader ->
      { wall with leader }

let list_to =
  List.fold_left apply_to
