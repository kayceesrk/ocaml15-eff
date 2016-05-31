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
  val yield   : unit -> unit
  val get_tid : unit -> int
end

module Queue = struct
  type 'a t = 'a list ref
  let create () = ref []
  let push v q = q := v::!q
  let pop q = 
    match !q with
    | [] -> raise Not_found
    | x::xs -> q := xs; x
  let length q = List.length !q
  let is_empty q = length q = 0
end

module Make (S : SCHED) : S = struct

  module Lock : sig
    type t
    val create : unit -> t
    val acq      : t -> unit
    val rel      : t -> unit
  end = struct

    type t = int ref

    let create () = ref 0

    let compare_and_swap r x y =
     ( Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y))

     let rec acq l = 
       let rec loop = function
         | 0 -> (S.yield (); loop 1024)
         | n ->
             if !l == 1 then loop (n - 1)
             else if compare_and_swap l 0 1 then ()
             else loop 1024
       in 
       loop 1024

     let rel l = 
       if compare_and_swap l 1 0 then ()
       else failwith "Lock.rel"
  end

  (** The state of mvar is either [Full v q] filled with value [v] and a queue
      [q] of threads waiting to fill the mvar, or [Empty q], with a queue [q] of
      threads waiting to empty the mvar. *)
  type 'a mv_state =
    | Full  of 'a * ('a * unit S.cont) Queue.t
    | Empty of 'a S.cont Queue.t

  type 'a t =
    {state : 'a mv_state ref;
     lock  : Lock.t; 
     id    : int}

  let next_mvar_id = CAS.ref 0

  let create_empty () =
    {state = ref (Empty (Queue.create ()));
     lock = Lock.create ();
     id = CAS.incr next_mvar_id}

  let create v =
    {state = ref (Full (v, Queue.create ()));
     lock = Lock.create ();
     id = CAS.incr next_mvar_id}

  let acq = Lock.acq
  let rel = Lock.rel 

  let put v mv =
    acq mv.lock;
    match !(mv.state) with
    | Full (v', q) ->
        S.suspend (fun k ->
          Queue.push (v,k) q;
          (* Printf.printf "[%d,%d] mv=%d Full q_len=%d\n%!" (Domain.self ()) tid mv.id @@ Queue.length q; *)
          rel mv.lock;
          None)
    | Empty q ->
        if Queue.is_empty q then begin
          mv.state := Full (v, Queue.create ());
          (* Printf.printf "[%d,%d] mv=%d Full q_len=%d\n%!" (Domain.self ()) tid mv.id 0; *)
          rel mv.lock
        end else
          let t = Queue.pop q in
          (* Printf.printf "[%d,%d] mv=%d Empty q_len=%d\n%!" (Domain.self ()) tid mv.id @@ Queue.length q; *)
          rel mv.lock;
          S.resume t v

  let take mv =
    acq mv.lock;
    match !(mv.state) with
    | Empty q ->
        S.suspend (fun k ->
         Queue.push k q;
         (* Printf.printf "[%d,%d] mv=%d Empty q_len=%d\n%!" (Domain.self ()) tid mv.id @@ Queue.length q; *)
         rel mv.lock;
         None)
    | Full (v, q) ->
        if Queue.is_empty q then begin
          mv.state := Empty (Queue.create ());
          (* Printf.printf "[%d,%d] mv=%d Empty q_len=%d\n%!" (Domain.self ()) tid mv.id 0; *)
          rel mv.lock;
          v
        end else
          let (v', t) = Queue.pop q in
          (mv.state := Full (v', q);
           (* Printf.printf "[%d,%d] mv=%d Full q_len=%d\n%!" (Domain.self ()) tid mv.id @@ Queue.length q; *)
           rel mv.lock;
           S.resume t ();
           v)
end
