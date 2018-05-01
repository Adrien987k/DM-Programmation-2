
let y = 4

let f x = y

let _ = f 5

let rec f x =
  if x = 0
  then 1
  else x * (f (x- 1))

let _ = f 5

let y = 4

let f = fun x -> fun y -> x y

let _ = f (fun x -> y) 3



