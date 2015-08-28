effect Foo : int -> int

let f () = (perform (Foo 3)) (* 3 + 1 *)
         + (perform (Foo 3)) (* 3 + 1 *)

let r =
  try
    f ()
  with effect (Foo i) k ->
    (* continuation called outside try/with *)
    continue k (i + 1)

let () = Printf.printf "%d\n" r
