open Core_kernel.Std
open Async_kernel.Std
include JsooMonad.S with type +'a t = 'a Or_error.t Deferred.t
