exception Foo of int

let f () = (raise (Foo 3)) + 1

let r =
  try
    f ()
  with Foo i -> i + 1
