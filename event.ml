type t =
  | Attack of Attack.t list
  | Blessing of Deity.t * Outcome.t
  | Deity of Deity.t list
  | Nations of int * Nation.t list
  | Over
  | Starting of Outcome.t
  | Support of (Nation.t * Outcome.t) list
  | Turn of int
  | Upkeep of Outcome.t

let to_wall wall = function
  | Attack alist ->
      List.map (fun a -> a.Attack.outcome) alist
      |> Outcome.list_to wall
  | Blessing (_, o)
  | Starting o
  | Upkeep o ->
      Outcome.apply_to wall o
  | Support ls ->
      List.map (fun (_, o) -> o) ls
      |> Outcome.list_to wall
  | Deity _
  | Nations _
  | Over
  | Turn _ -> wall
