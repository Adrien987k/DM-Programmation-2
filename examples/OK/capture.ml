let y = 4

let f x y = x y

let _ = f (fun x -> y) 3 (* should be 4 *)
