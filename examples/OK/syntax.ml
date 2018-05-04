(* Example of well-formed programs *)

(*
let t =
  let rec funct arg =
    if arg = 0 then 1
    else arg * (funct (arg - 1))
  in
  funct 5
*)
(*
let (t:int) =
  let rec funct =
    ((fun c ->
        let d = fst c in
        let arg = snd c in
        let funct = snd d in
        if arg = 0 then 1
        else arg *
             let b = funct in
             (fst b) ((b, arg - 1))), funct)
  in
  let a = funct in
  (fst a) ((a,5))
*)


let rec funct arg =
  if arg = 0 then 8
  else funct (arg - 1)

let _ = funct 5

(*
let rec (t:int -> int) =
  (fun b ->
     let c = fst b in
     let a = snd b in
     if a = 0 then 8 else
       let a = t in
       (fst a) ((a,a - 1)),
       ()) 

let (_:int) =
  let d = t in 
  (fst d) ((d,5))
*)

(*
let rec f x =
  if x = 0 then
    1
  else
    x * f (x - 1)

let _ = f 5
*)

(* WORKING
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