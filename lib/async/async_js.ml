(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2015 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
open Core_kernel.Std
open Async_kernel.Std
module Time_ns = Core_kernel.Time_ns
module Scheduler = struct
  include Async_kernel.Scheduler
  let can_run_a_job _ = false
end
let sleep d =
  let t = Async_kernel.Ivar.create () in
  let _id = Dom_html.setTimeout (Async_kernel.Ivar.fill t) (d *. 1000.) in
  Async_kernel.Ivar.read t

let yield () = sleep 0.

let run =
  let running = ref false in
  let rec loop () =
    if not !running
    then
      begin
	let t = Scheduler.t () in
	running:=true;
	Scheduler.run_cycle t;
	while Scheduler.can_run_a_job t do
	  Scheduler.run_cycle t
	done;
	let next_wakeup =
       	  match Scheduler.next_upcoming_event t with
          | None -> Some 0.1
	  | Some next ->
	    let now = Time_ns.now () in
	    let d = Time_ns.diff next now in
	    let ms = Time_ns.Span.to_ms d in
	    Some (min ms 0.1)
	in
	begin match next_wakeup with
	| None -> ()
	| Some next_wakeup -> ignore(Dom_html.setTimeout loop next_wakeup) end;
	running:=false
      end
  in
  loop
(*
let _ = Scheduler.set_new_job_hook run
*) 
