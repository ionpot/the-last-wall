type t =
  { max : int;
    chosen : Nation.t list;
  }

let make max =
  { max;
    chosen = [];
  }

let update ns chosen =
  let ls = Listx.undupe chosen in
  let len = List.length ls in
  if len = 0
  then ns
  else 
    { ns with
      chosen =
        if len > ns.max
        then Listx.pick_first ns.max ls
        else ls
    }
