(* The monad signature *)
module type MONAD =
sig
  type +_ t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Build reify and reflect operations for any monad *)
module RR(M: MONAD) :
sig
  val reflect : 'a M.t -> 'a
  val reify : (unit -> 'a) -> 'a M.t
end =
struct
  effect E : 'a M.t -> 'a
  let reflect m = perform (E m)
  let reify f = match f () with
      x -> M.return x
    | effect (E m) k -> M.bind m (continue k)
end

module State = struct
  type 'a t = int -> int * 'a
  let return v s = (s, v)
  let bind m k s = let s, a = m s in k a s
  (* val get : int t *)
  let get s = (s, s)
  (* val put : int -> unit t *)
  let put s _ = (s, ())
  let run s ~init = s init
end

(* Reify and reflect for State *)
module StateR = RR(State)

(* val put : int -> unit *)
let put v = StateR.reflect (State.put v)

(* val get : unit -> int *)
let get () = StateR.reflect State.get

(* val run_state : (unit -> 'a) -> init:int -> 'a *)
let run_state f ~init =
  let final, v = State.run (StateR.reify f) ~init in
  Printf.printf "Final state: %d\n" final;
  v

(* Using the state monad *)
let state_example () =
  let initial = get () in
  Printf.printf "Initial state: %d\n" initial;
  put 10;
  assert (get () = 10);
  put (get () + 1);
  assert (get () = 11);
  put 12;
  (`Initial initial, `Final (get ()))

let () =
  run_state ~init:10 state_example;
  print_endline "========================================";
