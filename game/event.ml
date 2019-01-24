module type Check = State.S -> sig val value : bool end

module type Base = sig
  type t
  module Make : State.S -> sig val value : t end
end

module type Direct = sig
  include Base
  module Apply : State.S -> sig val value : t -> unit end
end

module type Conditional = sig
  include Direct
  module Check : Check
end

module type Notify = sig
  include Base
  module Check : Check
end

module Always : Check = functor (_ : State.S) -> struct
  let value = true
end

module Never : Check = functor (_ : State.S) -> struct
  let value = false
end
