(* Example of well-formed programs *) 

let g x y = (y,x)

let h (x:int) y = x

let i x (y:int) = x

let j (x:int) (y:bool) = x

let arith = 1 + 2 + 3 + 4

let arith2 = 1 + 4 * 5 + 4

let test = if true then false else true

let foo =
  let x = let y = 4 in y
  in
  let z = (let y = 3 in y)
  in
  (x,z)

let foo' = (2+2,4)

let h x y = x

let test y = h y

let _ = (fun y -> (fun x -> fun y -> x) y) 2 3

let y = 4

let f x y = x y

let _ = f (fun x -> y) 3 (* should be 4 *)

