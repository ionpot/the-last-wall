type event =
  | Ph1 of Phase1.output
  | Ph2 of Phase2.output
  | Ph3 of Phase3.output
  | End

module First : State.S -> struct
  val value : event
end

module Next : State.S -> struct
  val value : event -> event
end
