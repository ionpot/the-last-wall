type event =
  | Ph1 of Phase1.output
  | Ph2 of Phase2.output
  | Ph3 of Phase3.output
  | End

type _ step =
  | One : Phase1.output step
  | Two : Phase2.output step
  | Three : Phase3.output step

let phase_of : type a. a step -> (module Phase.S with type output = a) =
  function
    | One -> (module Phase1)
    | Two -> (module Phase2)
    | Three -> (module Phase3)

module Apply (S : State.S) = struct
  let value : type a. a -> a step -> unit =
    fun x s ->
      let module P = (val phase_of s) in
      let module M = Seek.Apply(P)(S) in
      M.value x
end

module FirstStep (S : State.S) = struct
  let value : type a. a step -> a Seek.t =
    fun s ->
      let module P = (val phase_of s) in
      let module M = Seek.First(P)(S) in
      M.value
end

module NextStep (S : State.S) = struct
  let value : type a. a Seek.t -> a step -> a Seek.t =
    fun x s ->
      let module P = (val phase_of s) in
      let module M = Seek.Next(P)(S) in
      M.value x
end

module Make (S : State.S) = struct
  module F = FirstStep(S)
  let rec seek : type a. (a step -> a Seek.t) -> a step -> event =
    fun f p ->
      let next n = seek F.value n in
      match f p, p with
      | N.This e, One -> Ph1 e
      | N.This e, Two -> Ph2 e
      | N.This e, Three -> Ph3 e
      | N.Next, One -> next Two
      | N.Next, Two -> next Three
      | N.Next, Three -> next Two
      | N.End, _ -> End

  let first_of p =
    seek F.value p

  let next_of : type a. a -> a step -> event =
    fun x p ->
      let module N = NextStep(S) in
      seek (N.value x) p
end

module First (S : State.S) = struct
  module M = Make(S)
  let value = M.first_of One
end

module Next (S : State.S) = struct
  let next_of x p =
    let module A = Apply(S) in
    let module M = Make(S) in
    A.value x p;
    M.next_of x p

  let value = function
    | Ph1 x -> next_of x One
    | Ph2 x -> next_of x Two
    | Ph3 x -> next_of x Three
    | End -> End
end
