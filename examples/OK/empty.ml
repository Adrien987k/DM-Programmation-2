
(*
let x = 5

let f = (fun x -> x)
*)
let y = 4

let f = fun x -> fun y -> x y


let h = f (fun x -> y)

let _ = h 3

let g = fun h -> h