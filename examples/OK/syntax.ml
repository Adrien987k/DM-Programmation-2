(* Example of well-formed programs *)

let rec f x =
  if x = 0 then
    1
  else
    x * f (x - 1)

let _ = f 1

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
