open Core_kernel.Std
open Async_kernel.Std

type +'a t = 'a Or_error.t Deferred.t
type 'a w = 'a Or_error.t Ivar.t 
let bind = Deferred.Or_error.bind
let return = Deferred.Or_error.return
let task () =
  let ivar = Ivar.create () in
  Ivar.read ivar, ivar
let fill w x = Ivar.fill w (Ok x)
let don't_wait f = Deferred.don't_wait_for (f () >>| ignore)
let cancel t = assert false
let sleep d = Async_js.sleep d >>| fun x -> Ok x 
let on_cancel t f = Deferred.upon t (function | Error _ -> f () | Ok _ -> ())
let choose l =
  Deferred.choose (
    List.map l ~f:(fun x -> Deferred.choice x (fun x -> x))
    )
let iter f l = Deferred.List.iter l ~f:(fun x -> (f x >>| ignore)) >>| fun x -> Ok x 
