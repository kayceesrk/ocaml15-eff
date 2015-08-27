type action =
  | Atom of zaction
  | Fork of zaction * zaction
  | Yield of zaction
  | Suspend
  | Resume of zaction * zaction
  | Stop

and zaction = action lazy_t

type 'a t = ('a -> action) -> action

type 'a cont = 'a -> action

let (>>=) f k = fun c -> f (fun a -> k a c)
let (>>) a b = a >>= (fun _ -> b)
let return x = fun c -> c x
let atom f = fun c -> Atom (lazy (let b = f () in c b))
let action f = f (fun () -> Stop)
let fork f = fun c -> Fork (lazy (action f), lazy (c ()))
let stop = fun c -> Stop
let yield = fun c -> Yield (lazy (c ()))
let suspend f = fun c ->
  match f c with
  | None -> Suspend
  | Some (v, None) -> c v
  | Some (v, Some l) -> Resume (lazy(c v), l)

type ready_cont = zaction
let prepare k v = lazy (k v)


open Printf

let rec round = function
    | [] -> ()
    | (x::xs) -> match x with
        | Atom th -> let y = Lazy.force th in round (xs @ [y])
        | Fork (a1, a2) -> round (Lazy.force a1 :: Lazy.force a2 :: xs)
        | Yield a -> round ( xs @ [Lazy.force a])
        | Suspend -> round xs
        | Resume (a1, a2) -> round (Lazy.force a1 :: Lazy.force a2 :: xs)
        | Stop -> round xs

let run m = round [action m]

let rec iter_p f l =
  match l with
    | [] -> return ()
    | x :: l ->
        let tx = f x and tl = iter_p f l in
        tx >>= fun () -> tl

let map f m = (>>=) m (fun x -> return (f x))
let (>|=) t f = map f t

let rec map_p f l =
  match l with
  | [] -> return []
  | x :: l ->
    let tx = f x and tl = map_p f l in
    tx >>= fun x ->
    tl >|= fun l ->
    x :: l
