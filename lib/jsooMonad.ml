module type S = sig
    type +'a t
    type 'a w
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val task : unit -> 'a t * 'a w
    val don't_wait : (unit -> unit t) -> unit
    val fill : 'a w -> 'a -> unit
    val cancel : 'a w -> unit
    val sleep : float -> unit t
    val on_cancel : 'a t -> (unit -> unit) -> unit
    val choose : 'a t list -> 'a t 
    val iter : ('a -> unit t) -> 'a list -> unit t 
end
