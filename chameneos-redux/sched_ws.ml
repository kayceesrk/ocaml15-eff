(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  type 'a cont
  val suspend : ('a cont -> 'a option) -> 'a
  val resume  : 'a cont -> 'a -> unit
  val fork    : (unit -> unit) -> unit
  val fork_rr : (unit -> unit) -> unit
  val fork_on : (unit -> unit) -> int -> unit
  val yield   : unit -> unit
  val get_tid : unit -> int
  val run     : (unit -> unit) -> unit
end

module Make (S : sig val num_domains : int end) : S = struct

  module CAS = CAS_l
  module MSQueue = MSQueue_l
  module Backoff = Backoff_l


  type queue_num = int
  type 'a cont = ('a, unit) continuation * queue_num

  effect Fork     : (unit -> unit) -> unit
  effect Yield    : unit
  effect Suspend  : ('a cont -> 'a option) -> 'a
  effect Resume   : ('a cont * 'a) -> unit
  effect GetTid   : int

  let fork f      = perform (Fork f)
  let yield ()    = perform Yield
  let suspend f   = perform (Suspend f)
  let resume t v  = perform (Resume (t, v))
  let get_tid ()  = perform GetTid

  effect ForkOn     : (unit -> unit) * int -> unit

  (* XXX unsafe with multiple schedulers *)
  let next_domain = ref 0
  let incr x =
    let old = !x in
    x := succ !x;
    old

  open CAS.Sugar

  let fork_on f dom_id = perform (ForkOn (f, dom_id))
  let fork_rr f =
    perform (ForkOn (f, incr next_domain mod S.num_domains))
  let num_domains () = S.num_domains



  let sq = Array.init S.num_domains (fun _ -> MSQueue.create ())

  let fresh_tid () = Oo.id (object end)

  let enqueue c dom_id = MSQueue.push (Array.get sq dom_id) c

  let rec num_threads = ref 0

  and dequeue_wid dom_id =
    let b = Backoff.create () in
    let queue = Array.get sq dom_id in
    let rec loop () = match MSQueue.pop queue with
      | Some k -> continue k ()
      | None ->
          if !num_threads = 0 then ()
          else ( Backoff.once b ; loop () )
    in loop ()

  and dequeue () = dequeue_wid (Domain.self ())

  and spawn g (tid:int) =
    CAS.incr num_threads;
    begin
      match g () with
      | () -> (CAS.decr num_threads; dequeue ())
      | effect (Fork f) k -> enqueue k (Domain.self ()); spawn f (fresh_tid ())
      | effect Yield k -> enqueue k (Domain.self ()); dequeue ()
      | effect (Suspend f) k ->
          begin
            match f (k, Domain.self()) with
              | None -> dequeue ()
              | Some v -> continue k v
              | effect (Resume ((t, qid), v)) k -> enqueue k qid; continue t v
          end
      | effect (Resume ((t,qid), v)) k -> enqueue k qid; continue t v
      | effect GetTid k -> continue k tid
      | effect (ForkOn (f, dom_id)) k ->
          (enqueue k dom_id;
           Printf.printf "ForkOn: %d\n%!" dom_id;
           spawn f (fresh_tid ()))
    end

  let run_with f num_domains =
    let started = ref 0 in
    let worker () =
      let b = Backoff.create ~max:16 () in
      let rec loop () =
        if !started = 1 then dequeue ()
        else (Backoff.once b; loop ())
      in loop ()
    in
    for i = 1 to num_domains - 1 do
      Domain.spawn worker
    done ;
    spawn (fun () -> CAS.incr started; f ()) (fresh_tid ())

  let run f = run_with f S.num_domains

end
