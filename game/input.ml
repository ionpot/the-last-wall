open Input_event

type 'a input = 'a Event.Input.t

type kind =
  | DeityChoice of DeityChoice.t input
  | LeaderChoice of LeaderChoice.t input
(*
  | Barracks of Barracks.t t
  | Barrage of Barrage.t t
  | BarrageTrain of BarrageTrain.t t
  | Build of Build.t t
  | LeaderNew of LeaderNew.t t
  | MercsEnd of MercsEnd.t t
  | Nations of Nations.t t
  | Research of Research.t t
  | Scout of Scout.t t
  | Sodistan of Sodistan.t t
  | Temple of Temple.t t
  | Trade of Trade.t t
  | Volunteers of Volunteers.t t
  *)
type 'a convert = 'a input -> kind
type 'a cond = 'a Event.cond * 'a convert
type 'a direct = 'a Event.direct * 'a convert

let of_direct state =
  let module Direct = Steps.Input in
  let make = Event.Input.make in
  function
  | Direct.Deity -> DeityChoice (make (module DeityChoice) state)
  | Direct.Leader -> LeaderChoice (make (module LeaderChoice) state)
(*
  | Direct.BarrageTrain -> (module BarrageTrain), (fun x -> BarrageTrain x)
  | Direct.Build -> (module Build), (fun x -> Build x)
  | Direct.MercsEnd -> (module MercsEnd), (fun x -> MercsEnd x)
  | Direct.Nations -> (module Nations), (fun x -> Nations x)
  | Direct.Research -> (module Research), (fun x -> Research x)
  | Direct.Scout -> (module Scout), (fun x -> Scout x)
  | Direct.Sodistan -> (module Sodistan), (fun x -> Sodistan x)
*)
  | _ -> failwith "todo"

let of_cond : Steps.Input.cond -> 'a cond =
  let module Cond = Steps.Input in
  function
(*
  | Cond.Barracks -> (module Barracks), (fun x -> Barracks x)
  | Cond.Barrage -> (module Barrage), (fun x -> Barrage x)
  | Cond.LeaderNew -> (module LeaderNew), (fun x -> LeaderNew x)
  | Cond.Temple -> (module Temple), (fun x -> Temple x)
  | Cond.Trade -> (module Trade), (fun x -> Trade x)
  | Cond.Volunteers -> (module Volunteers), (fun x -> Volunteers x)
*)
  | _ -> failwith "todo"
