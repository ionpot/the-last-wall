type t =
  { deity : Deity.t;
    max_nations : int;
    nations : Nation.t list;
    wall : Wall.t;
  }

let make deity wall =
  { deity;
    max_nations = 3;
    nations = []
    wall;
  }
