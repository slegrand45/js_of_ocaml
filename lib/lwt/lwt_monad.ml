
type +'a t = 'a Lwt.t
type 'a w = 'a Lwt.u

let bind = Lwt.bind
let return = Lwt.return
let task () =
  let t,w = Lwt.task () in
  t, w
let fill w x = Lwt.wakeup w x
let don't_wait f = Lwt.async f
let cancel t = Lwt.cancel (Obj.magic t)
let sleep d = Lwt_js.sleep d
let on_cancel t f = Lwt.on_cancel t f
let choose l = Lwt.choose l
let iter l f = Lwt_list.iter_s l f 
