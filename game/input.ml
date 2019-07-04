module Ballista = struct
  type t = Defs.count * Defs.count
  let kind = Units.Ballista
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value (n, _) =
      Recruit.sub_cost kind n;
      let n' = S.Ballista.get () in
      S.Units.map (Units.add n' kind);
      S.Ballista.set n
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let have = S.Units.return (Units.count kind)
    let total = have + S.Ballista.get ()
    let cap = S.Build.return Build.ballista_cap
    let avlb = Recruit.affordable kind (cap - total)
    let value = avlb, total
  end
end

module Barrage = struct
  type t = bool
  module Apply (S : State.S) = struct
    let value = S.Barraging.set_to
  end
  module Check (S : State.S) = struct
    let value = S.Barraging.get ()
  end
  module Make (S : State.S) = struct
    let value = S.Barraging.get ()
  end
end

module BuildAvlb = struct
  type t = Build.kind list
  module Apply (S : State.S) = struct
    module Bonus = Build_bonus.From(S)
    let value ls = S.Build.map (Build.start ls Bonus.value)
  end
  module Make (S : State.S) = struct
    let value = S.Build.return Build.ls_avlb
  end
end

module DeityChoice = struct
  type t = Deity.t
  module Apply (S : State.S) = struct
    let value = S.Deity.set
  end
  module Make (S : State.S) = struct
    let value = Deity.empty
  end
end

module Dervish = struct
  type t = Defs.count
  let kind = Units.Dervish
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value n =
      Recruit.promote kind n;
      S.Dervish.set n
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let cap = Recruit.(Missing.temple () |> affordable kind)
    let a, b = Recruit.dervish_range ()
    let value = S.Dice.between_try a (min b cap)
  end
end

module Knight = struct
  type t = Defs.count
  let kind = Units.Knight
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promotable kind
  end
end

module LeaderKind = struct
  type t = Leader.kind
  module Apply (S : State.S) = struct
    module Roll = Leader.Roll(S.Dice)
    let value kind = S.Leader.set (Roll.from kind)
  end
  module Make (S : State.S) = struct
    let value = Leader.(kind_of empty)
  end
end

module LeaderNew = struct
  type t = Leader.t list
  module Apply (S : State.S) = struct
    let value = function
      | [] -> ()
      | ldr :: _ ->
          let level = Leader.level_of ldr in
          if S.Supply.has level then begin
            S.Supply.sub level;
            S.Leader.set ldr
          end
  end
  module Check (S : State.S) = struct
    let value = S.Turn.return Leader.can_respawn |> S.Leader.check
  end
  module Make (S : State.S) = struct
    module Roll = Leader.Roll(S.Dice)
    let value = Roll.random ()
  end
end

module Mercs = struct
  type t = Defs.count
  let kind = Units.Men
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Check (S : State.S) = struct
    let value = S.Dice.chance 0.8
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let cap = S.Dice.between 10 30
    let value = Recruit.supply_limit kind cap
  end
end

module Nations = struct
  type t = Nation.kind list
  module Apply (S : State.S) = struct
    let value ls = S.Nation.map (Nation.chosen ls)
  end
  module Make (S : State.S) = struct
    let value = S.Nation.return Nation.which
  end
end

module Ranger = struct
  type t = Defs.count
  let kind = Units.Ranger
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Check (S : State.S) = struct
    let value = S.Deity.is Deity.Sitera
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promotable kind
  end
end

module Scout = struct
  type t = bool
  module Apply (S : State.S) = struct
    let value = S.Scout.set_to
  end
  module Make (S : State.S) = struct
    let value = S.Scout.get ()
  end
end

module Templar = struct
  type t = Defs.count
  let kind = Units.Templar
  module Apply (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promote kind
  end
  module Check (S : State.S) = struct
    let value = not (S.Deity.is Deity.Sitera)
  end
  module Make (S : State.S) = struct
    module Recruit = Recruit.With(S)
    let value = Recruit.promotable kind
  end
end

module Trade = struct
  type t = Nation.trade
  let none = Nation.NoTrade
  module Apply (S : State.S) = struct
    let value trade =
      S.Build.map (Build.set_trade trade)
  end
  module Check (S : State.S) = struct
    let value = S.Build.check Build.(built (Trade none))
  end
  module Make (S : State.S) = struct
    let value = none
  end
end
