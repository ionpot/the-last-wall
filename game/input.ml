open Input_event

type 'a apply = State.t -> 'a -> State.t
type 'a input = 'a * 'a apply

type kind =
  | DeityChoice of DeityChoice.t input
  | LeaderChoice of LeaderChoice.t input
  | Nations of Nations.t input
  | Trade of Trade.t input
(*
  | Barracks of Barracks.t t
  | Barrage of Barrage.t t
  | BarrageTrain of BarrageTrain.t t
  | Build of Build.t t
  | LeaderNew of LeaderNew.t t
  | MercsEnd of MercsEnd.t t
  | Research of Research.t t
  | Scout of Scout.t t
  | Sodistan of Sodistan.t t
  | Temple of Temple.t t
  | Volunteers of Volunteers.t t
  *)

let direct : type a. a Event.direct -> (a input -> kind) -> State.t -> kind =
  fun (module M) f state ->
    f (M.make state, fun s x -> M.apply x s)

let cond : type a. a Event.cond -> (a input -> kind) -> State.t -> kind option =
  fun (module M) f ->
    let g = direct (module M) f in
    Event.cond (module M) g

let of_direct =
  let module Direct = Steps.Input in
  function
  | Direct.Deity -> direct (module DeityChoice) (fun x -> DeityChoice x)
  | Direct.Leader -> direct (module LeaderChoice) (fun x -> LeaderChoice x)
  | Direct.Nations -> direct (module Nations) (fun x -> Nations x)
(*
  | Direct.BarrageTrain -> (module BarrageTrain), (fun x -> BarrageTrain x)
  | Direct.Build -> (module Build), (fun x -> Build x)
  | Direct.MercsEnd -> (module MercsEnd), (fun x -> MercsEnd x)
  | Direct.Research -> (module Research), (fun x -> Research x)
  | Direct.Scout -> (module Scout), (fun x -> Scout x)
  | Direct.Sodistan -> (module Sodistan), (fun x -> Sodistan x)
*)
  | _ -> failwith "todo"

let of_cond =
  let module Cond = Steps.Input in
  function
  | Cond.Trade -> cond (module Trade) (fun x -> Trade x)
(*
  | Cond.Barracks -> cond (module Barracks) (fun x -> Barracks x)
  | Cond.Barrage -> (module Barrage), (fun x -> Barrage x)
  | Cond.LeaderNew -> (module LeaderNew), (fun x -> LeaderNew x)
  | Cond.Temple -> (module Temple), (fun x -> Temple x)
  | Cond.Volunteers -> (module Volunteers), (fun x -> Volunteers x)
*)
  | _ -> failwith "todo"
