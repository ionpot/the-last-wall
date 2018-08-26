type t = Alive of Leader.t | Wait of Defs.turn

let empty = Wait 0
let dead = Wait 1

let get = function
  | Alive ldr -> Some ldr
  | Wait _ -> None
let make ldr = Alive ldr
let need t = t = Wait 0
let tick = function
  | Alive _
  | Wait 0 as x -> x
  | Wait x -> Wait (x - 1)
