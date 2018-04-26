
let y = 4

let f = fun x -> fun y -> x y

let _ = f (fun x -> y) 3