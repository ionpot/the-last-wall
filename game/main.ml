type step =
  | Next of Step.t
  | End

let make = function
  | Some step -> Next step
  | None -> End

let next steps state =
  Step.next state steps |> make

let first = next Steps.start
