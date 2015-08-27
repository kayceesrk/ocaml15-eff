type 'a t
val new_mvar : 'a -> 'a t
val new_empty_mvar : unit -> 'a t
val put_mvar : 'a t -> 'a -> unit Sched_monad.t
val take_mvar : 'a t -> 'a Sched_monad.t
