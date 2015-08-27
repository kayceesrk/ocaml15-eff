module S = Sched_monad

(** The state of mvar is either [Full v q] filled with value [v] and a queue
    [q] of threads waiting to fill the mvar, or [Empty q], with a queue [q] of
    threads waiting to empty the mvar. *)
type 'a mv_state =
  | Full  of 'a * ('a * unit S.cont) Queue.t
  | Empty of 'a S.cont Queue.t

type 'a t = 'a mv_state ref

let new_empty_mvar () = ref (Empty (Queue.create ()))

let new_mvar v = ref (Full (v, Queue.create ()))

let put_mvar mv v =
  S.suspend ( fun k ->
  match !mv with
  | Full (v', q) ->
      Queue.push (v,k) q;
      None
  | Empty q ->
      if Queue.is_empty q then
        ( mv := Full (v, Queue.create ());
          Some ((), None))
      else
        let t = Queue.pop q in
        Some ((), Some (S.prepare t v)) )

let (>>) = S.(>>)

let take_mvar mv =
  S.suspend (fun k ->
  match !mv with
  | Empty q ->
      Queue.push k q;
      None
  | Full (v, q) ->
      if Queue.is_empty q then
        (mv := Empty (Queue.create ());
         Some (v, None))
      else
        let (v', t) = Queue.pop q in
        mv := Full (v', q);
        Printf.printf "take_mvar: resume\n";
        Some (v, Some (S.prepare t ())))
