let p1 = [
  Do Starting;
  Ask Nations;
  Do Support;
  Ask Build;
  Try BuildSupply;
  Ask Scouts
]

let p2 = [
  Do Turn;
  Try BuildMp;
  Do BuildTick;
  Nfy Built;
  Nfy Needs;
  Try NewLdr;
  Do Upkeep;
  Try Starv;
  Nfy Defeat;
  Nfy Report;
  Try Bless;
  Try Market;
  Ask Nats;
  Do Support;
  Do Build;
  Try BuildSp;
  Try Cavalry;
  Try Mercs
]

let p3 = [
  JmpIfNo (Nfy Atk, Last);
  Try Smite;
  Ask Barrage;
  Try Barraged;
  Do Casualty;
  Try Fort;
  Nfy Defeat;
  Try Victory;
  Try LdrDead;
  Try LdrLvUp;
  Ask Scouts
]
