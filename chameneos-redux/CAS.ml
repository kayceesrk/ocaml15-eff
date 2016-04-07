(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
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

type 'a updt = { expect : 'a ; update : 'a }

type 'a state =
  | Idle of 'a
  | InProgress of 'a * int * t list

and 'a ref = { mutable content : 'a state ;
                        id      : int      }

and t = CAS : 'a ref * 'a updt -> t

let get_id {id; _} = id

let compare_and_swap r x y =
  ( Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y))

let ref x = { content = Idle x           ;
              id      = Oo.id (object end) }

let get r = match r.content with
  | Idle a -> a
  | InProgress (a,_,_) -> a


let cas r u = CAS (r, u)

let is_on_ref (CAS (r1, _)) r2 = r1.id == r2.id

let get_cas_id (CAS ({id;_},_)) = id

let commit (CAS (r, { expect ; update })) =
  let s = r.content in
  match s with
  | Idle a when a == expect ->
      if expect == update then true
      else compare_and_swap r s (Idle update)
  | _ -> false

let rec semicas id = function
  | [] -> true
  | (CAS (r, { expect ; _ }))::xs ->
      let s = r.content in
      match s with
      | Idle a ->
          if a == expect then
            if compare_and_swap r s (InProgress (a,id,xs)) then
              semicas id xs (* continue with the rest of the CASes *)
            else false (* CAS has failed *)
          else false (* CAS will fail *)
      | InProgress (_,id',k') ->
          if id = id'
          then false (* Multiple compare and swaps on the same location. *)
          else (* Race lost! Help complete the victorious thread's kCAS *)
            ( ignore (semicas id' k'); false )

(* Only the thread that performed the semicas should be able to rollbwd/fwd.
 * Hence, we don't need to CAS. *)
let rollbwd (CAS (r, _)) =
  match r.content with
  | Idle _      -> ()
  | InProgress (x,_,_) -> r.content <- Idle x

let rollfwd (CAS (r, { update ; _ })) =
  match r.content with
  | Idle _ -> failwith "CAS.kCAS: broken invariant"
  | InProgress (x,_,_) ->  r.content <- Idle update
                    (* we know we have x == expect *)

let kCAS l =
  let l = List.sort (fun c1 c2 -> compare (get_cas_id c1) (get_cas_id c2)) l in
  let id = Oo.id (object end) in
  if semicas id l then
    ( List.iter rollfwd l ; true )
  else
    ( List.iter rollbwd l ; false )

module Sugar : sig
  val ref : 'a -> 'a ref
  val (!) : 'a ref -> 'a
  val (-->) : 'a -> 'a -> 'a updt
  val (<!=) : 'a ref -> 'a updt -> bool
  val (<:=) : 'a ref -> 'a updt -> t
end = struct
  type 'a casupdt = 'a updt
  type 'a casref = 'a ref
  let ref x = ref x
  let (!) r = get r

  let (-->) expect update = { expect ; update }

  let (<:=) = cas
  let (<!=) r u = commit (cas r u)
end

open Sugar

type 'a cas_result = Aborted | Failed | Success of 'a

let try_map r f =
  let s = !r in
  match f s with
  | None -> Aborted
  | Some v ->
      if r <!= s --> v then Success s else Failed

let map r f =
  let b = Backoff.create () in
  let rec loop () =
    match try_map r f with
    | Failed -> (Backoff.once b; loop ())
    | v -> v
  in loop ()

let incr r = ignore @@ map r (fun x -> Some (x + 1))
let decr r = ignore @@ map r (fun x -> Some (x - 1))
