module type S = sig
  type 'a cont
  effect Suspend  : ('a cont -> unit) -> 'a
  effect Resume   : 'a cont * 'a -> unit
  val suspend : ('a cont -> unit) -> 'a
  val resume  : 'a cont -> 'a -> unit
  val fork    : (unit -> unit) -> unit
  val fork_on : (unit -> unit) -> int -> unit
  val yield   : unit -> unit
  val get_tid : unit -> int
  val run     : (unit -> unit) -> unit
end

module Make (S : sig val num_domains : int end) : S
