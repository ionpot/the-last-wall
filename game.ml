type t =
  { deity : Deity.t;
    nations : Nations.t;
    state : State.t;
    turn : int;
    wall : Wall.t;
  }

let make : unit -> t =
  fun () ->
    { deity = None;
      nations = Nations.make 3;
      state = State.first;
      turn = 0;
      wall = Wall.make ();
    }

let next_state : t -> State.t =
  fun game ->
    if Wall.is_destroyed game.wall
    then State.Over
    else
      let ls =
        if game.turn > 0
        then State.other_turns
        else State.first_turn
      in
      State.next_in ls game.state

let next : t -> t =
  fun game ->
    let state = next_state game in
    let turn =
      if state = Turn
      then succ game.turn
      else game.turn
    in
    { game with state; turn }

let update : Input.t -> t =
  fun game input ->
    match input with
    | Input.Deity deity ->
        { game with deity }
    | Input.Nations ls ->
        let nations = Nations.update game.nations ls in
        { game with nations }
    | Input.Event evt ->
        { game with
          wall = Event.to_wall game.wall evt;
        }

let event_of : t -> Event.t =
  fun game ->
    let nations = game.nations in
    match game.state with
    | Attack -> Attack (Attack.build game)
    | Blessing ->
        let d = game.deity in
        Blessing (d, Blessing.of_deity d)
    | Deity -> Deity (Deity.to_list ())
    | Nation -> Nations (nations.max, Nation.to_list ())
    | Over -> Over
    | Starting -> Starting (Starting.make ())
    | Support -> Support (Support.of_nations nations)
    | Turn -> Turn game.turn
    | Upkeep -> Upkeep (Upkeep.of_wall game.wall)
