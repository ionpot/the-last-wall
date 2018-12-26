type t = Alive of Leader.t | Need | Wait of Defs.turn

let dead = Wait 1

let get = function
  | Alive ldr -> Some ldr
  | Need
  | Wait _ -> None
let make ldr = Alive ldr
let need t = t = Need
let tick = function
  | Alive _
  | Need as x -> x
  | Wait 0 -> Need
  | Wait x -> Wait (x - 1)
