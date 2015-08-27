open Printexc

effect Fork  : (unit -> unit) -> unit
effect Yield : unit

(* type _ eff +=
   | Fork  : (unit -> unit) -> unit eff
   | Yield : unit eff *)

(* val perform : 'a eff -> 'a *)

let fork f = perform (Fork f)
let yield () = perform Yield

let s = 10

let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    (* val continue : ('a,'b) continuation -> 'a -> 'b *)
    else continue (Queue.pop run_q) ()
  in
  let rec spawn f =
    (* Runs in a new fiber *)
    match f () with
    | () -> dequeue ()
    | exception e ->
        ( print_string (to_string e);
          dequeue () )
    (* effect / ('a eff) / ('a,'b) continuation *)
    | effect Yield k ->
        ( enqueue k; dequeue () )
    | effect (Fork f) k ->
        ( enqueue k; spawn f )
  in
  spawn main
