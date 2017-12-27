type t =
  { deity : Deity.t;
    nations : Nation.t list;
    wall : Wall.t;
  }

let make deity wall =
  { deity; wall; nations = [] }
