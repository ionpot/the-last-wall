module Make (Set : Set.S) = struct
  type cost = Resource.t
  type t = (Set.elt * cost) list
  let empty : t = []
  let cost t =
    List.map snd t
    |> List.fold_left Resource.(++) Resource.empty
  let to_set t = List.map fst t |> Set.of_list
  let add kind cost t = (kind, cost) :: t
  let apply_if cond res (t:t) =
    Listx.rmap_with (fun rem (k, cost) ->
      let rem', cost' =
        if cond cost
        then Resource.deduce rem cost
        else rem, cost
      in rem', (k, cost')) res t
  let apply = apply_if (fun _ -> true)
  let rm_set s t = Listx.discard (fun (k, _) -> Set.mem k s) t
  let partition res t =
    let f x (rem, pass, fail) =
      let cost = snd x in
      if Resource.has cost rem
      then Resource.(rem -- cost), (x :: pass), fail
      else Resource.empty, pass, (x :: fail)
    in
    List.fold_right f t (res, [], [])
  let pop_finished t =
    let f (_, cost) = Resource.empty = cost in
    let yes, no = List.partition f t in
    List.map fst yes, no
end
