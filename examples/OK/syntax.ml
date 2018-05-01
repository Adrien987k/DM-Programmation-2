(* Example of well-formed programs *)

let test =
  let rec funct arg =
    if arg = 0 then 0
    else funct (arg - 1)
  in
  funct 5
(*
let test = 
  let funct = fix(
      ((fun e -> 
          let f = fst e in
          let funct = snd e in 
          ((fun c -> 
              let d = fst c in
              let arg = snd c in
              let funct = snd d in
              if arg = 0 then 0 else
                let b = funct in
                (fst b) ((b, arg - 1))), funct)),
       ())) 
  in
  let a = funct in
  (fst a) ((a,5))
*)
(*
let rec test arg =
  if arg = 0 then 0
  else test (arg - 1)

let _ = test 5
*)
(*
let test =
  let f = fix (
      ((fun e -> 
          let f = fst e in
          let f = snd e in
          ((fun c -> 
              let d = fst c in
              let x = snd c in
              let f = snd d in 
              let b = f in
              (fst b) ((b, x - 1))),f)),
       ()))
  in 
  let a = f in 
  (fst a) ((a,5))
*)
(*
let rec f x =
  if x = 0 then
    1
  else
    x * f (x - 1)

let _ = f 5

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

*)