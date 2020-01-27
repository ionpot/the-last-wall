module Make : State.S -> sig
  val artillery : Power.t -> Power.t
  val brg_coef : Defs.power -> Defs.power
  val brg_penalty : Power.t -> Power.t
  val brg_power : Power.t -> Power.t
  val build_cost : Build.kind -> Resource.t -> Resource.t
  val cav_allowed : Defs.ratio -> Defs.ratio
  val dr_leader : Defs.power -> Defs.power
  val dr_mausoleums : Defs.power -> Defs.power
  val dr_penalties : Defs.power -> Defs.power
  val dr_snow_penalty : Power.t -> Power.t
  val dr_wind_penalty : Power.t -> Power.t
  val market_boost : Build.kind -> Resource.t -> Resource.t
  val recruit_fast : Units.kind -> bool
  val recruit_sup : Units.kind -> Defs.supply -> Defs.supply
  val resource_disease : Resource.t -> Resource.t
  val siege_boost : Power.t -> Power.t
  val smite : Defs.power -> Defs.power
  val support_barracks : Nation.kind -> Resource.t -> Resource.t
  val support_hekatium : Nation.kind -> Resource.t -> Resource.t
  val support_trade : Nation.kind -> Resource.t -> Resource.t
  val support_winter : Defs.percent -> Defs.percent
  val temple_men : Defs.count -> Defs.count
  val upkeep_engr : Defs.supply -> Defs.supply
  val upkeep_scouting : Defs.supply -> Defs.supply
  val upkeep_units : Defs.supply -> Defs.supply
  val volunteers : Defs.count -> Defs.count
end
