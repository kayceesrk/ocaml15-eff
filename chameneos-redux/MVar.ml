module type S = sig
  type 'a t
  val create       : 'a -> 'a t
  val create_empty : unit -> 'a t
  val put       : 'a -> 'a t -> unit
  val take      : 'a t -> 'a
end

module type SCHED = sig
  type 'a cont
  val suspend : ('a cont -> 'a option) -> 'a
  val resume  : 'a cont -> 'a -> unit
end

module Make (S : SCHED) : S = struct

  module TS : Reagents.Scheduler = struct
    include S
    let get_tid () = 0
  end

  module Reagents = Reagents.Make(TS)
  module RS = Reagents_sync.Make(Reagents)
  module Lock = RS.Lock

  (** The state of mvar is either [Full v q] filled with value [v] and a queue
      [q] of threads waiting to fill the mvar, or [Empty q], with a queue [q] of
      threads waiting to empty the mvar. *)
  type 'a mv_state =
    | Full  of 'a * ('a * unit S.cont) Queue.t
    | Empty of 'a S.cont Queue.t

  type 'a t =
    {state : 'a mv_state ref;
     lock  : Lock.t}

  let create_empty () =
    {state = ref (Empty (Queue.create ()));
     lock = Lock.create ()}

  let create v =
    {state = ref (Full (v, Queue.create ()));
     lock = Lock.create ()}

  let acq l = Reagents.run (Lock.acq l) ()
  let rel l = ignore (Reagents.run (Lock.rel l) ())

  let put v mv =
    acq mv.lock;
    match !(mv.state) with
    | Full (v', q) ->
        S.suspend (fun k ->
          Queue.push (v,k) q;
          rel mv.lock;
          None)
    | Empty q ->
        if Queue.is_empty q then begin
          mv.state := Full (v, Queue.create ());
          rel mv.lock
        end else
          let t = Queue.pop q in
          rel mv.lock;
          S.resume t v

  let take mv =
    acq mv.lock;
    match !(mv.state) with
    | Empty q ->
        S.suspend (fun k ->
         Queue.push k q;
         rel mv.lock;
         None)
    | Full (v, q) ->
        if Queue.is_empty q then begin
          mv.state := Empty (Queue.create ());
          rel mv.lock;
          v
        end else
          let (v', t) = Queue.pop q in
          (mv.state := Full (v', q);
           rel mv.lock;
           S.resume t ();
           v)
end
