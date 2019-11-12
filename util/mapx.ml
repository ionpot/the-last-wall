module Make (Map : Map.S) = struct
  let mapk f t =
    Map.mapi (fun k _ -> f k) t

  let nth t n =
    let key, _ = Map.choose t in
    let f k _ (k', n') =
      if n' > 0 then k, pred n' else k', n'
    in
    Map.fold f t (key, n) |> fst

  module Float = struct
    type t = float Map.t

    let pick t p =
      let key, _ = Map.min_binding t in
      let f k v (k', p') =
        if p' > 0. then k, p' -. v else k', p'
      in
      Map.fold f t (key, p) |> fst
  end
end
