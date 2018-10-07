let map_to_list f q =
  Queue.fold (fun ls x -> x :: ls) [] q
  |> List.rev_map f
