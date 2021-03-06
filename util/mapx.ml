module Make (Map : Map.S) = struct
  let existsk f t =
    Map.exists (fun k _ -> f k) t

  let existsv f t =
    Map.exists (fun _ x -> f x) t

  let filterk f t =
    Map.filter (fun k _ -> f k) t

  let filterv f t =
    Map.filter (fun _ -> f) t

  let foldv f acc t =
    Map.fold (fun _ v acc -> f acc v) t acc

  let discardk f t =
    filterk (fun k -> not (f k)) t

  let keys t =
    Map.bindings t |> List.map fst

  let mapk f t =
    Map.mapi (fun k _ -> f k) t

  let map_mem k f t =
    Map.update k (function Some x -> Some (f x) | x -> x) t

  let maybe f v k t =
    if Map.mem k t then Map.find k t |> f else v

  let nth t n =
    let key, _ = Map.choose t in
    let f k _ (k', n') =
      if n' > 0 then k, pred n' else k', n'
    in
    Map.fold f t (key, n) |> fst

  module Float = struct
    type t = float Map.t

    let value k t =
      if Map.mem k t then Map.find k t else 0.

    let add_to k v t =
      Map.add k (value k t +. v) t

    let clean t =
      filterv ((<) 0.) t

    let max t =
      Map.fold (fun _ -> max) t 0.

    let pick t p =
      let key, _ = Map.min_binding t in
      let f k v (k', p') =
        if p' > 0. then k, p' -. v else k', p'
      in
      Map.fold f t (key, p) |> fst

    let sum t =
      Map.fold (fun _ -> (+.)) t 0.
  end

  module Int = struct
    type t = int Map.t

    let add t_a t_b =
      Map.union (fun _ a b -> Some (a + b)) t_a t_b

    let div t_a t_b =
      let f _ a_opt = function
        | Some b ->
            if b > 0
            then Some (Number.maybe 0 a_opt / b)
            else None
        | None -> None
      in
      Map.merge f t_a t_b

    let div_by n t =
      if n > 0 then Map.map (fun x -> x / n) t
      else Map.empty

    let min t =
      let cmp n = function
        | Some x -> Some (min x n)
        | None -> Some n
      in
      Map.fold (fun _ -> cmp) t None
      |> Number.maybe 0

    let mul_by n t =
      Map.map (( * ) n) t

    let sub t_a t_b =
      let f _ a_opt = function
        | Some b -> Number.(sub_opt (maybe 0 a_opt) b)
        | None -> a_opt
      in
      Map.merge f t_a t_b

    let sum t =
      Map.fold (fun _ -> (+)) t 0

    let value k t =
      if Map.mem k t then Map.find k t else 0

    let add_to k n t =
      Map.add k (value k t + n) t

    let sub_from k n t =
      Map.add k (value k t - n) t

    let pred k t =
      match Number.sub_opt (value k t) 1 with
      | Some x -> Map.add k x t
      | None -> Map.remove k t

    let succ k t =
      add_to k 1 t
  end

  let min t =
    Map.min_binding t |> snd
    |> Map.fold (fun _ -> min) t
end
