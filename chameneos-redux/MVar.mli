module type S = sig
  (** MVar type. Represents a data structure with a single hole that can be
      filled with value. *)
  type 'a t

  (** [create v] allocates a new mvar with the hole filled with value [v]. *)
  val create : 'a -> 'a t

  (** [create_empty ()] allocates a new mvar with the hole empty. *)
  val create_empty : unit -> 'a t

  (** [put v m] fills mvar [m] with value v. If the mvar is already filled,
      this operation blocks until the hole become empty. *)
  val put : 'a -> 'a t -> unit

  (** [take m] empties the mvar [m] if it is filled and returns the value.
      If [m] is empty, then the operation blocks until the mvar becomes filled. *)
  val take : 'a t -> 'a
end

module type SCHED = sig
  type 'a cont
  (** Represents a blocked computation that waits for a value of type 'a. *)

  val suspend : ('a cont -> 'a option) -> 'a
  (** [suspend f] applies [f] to the current continuation, and suspends the
      execution of the current thread, and switches to the next thread in the
      scheduler's queue. *)

  val resume  : 'a cont -> 'a -> unit
  (** [resume (k,v)] prepares the suspended continuation [k] with value [v] and
      enqueues it to the scheduler queue. *)

  val yield : unit -> unit
  val get_tid : unit -> int
end

module Make (S : SCHED) : S
