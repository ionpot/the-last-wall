type step =
  | Next of Step.t
  | End

let make = function
  | Some step -> Next step
  | None -> End

let next = function
  | Next step -> Step.next step |> make
  | End -> End

let first state =
  Step.first state |> make
