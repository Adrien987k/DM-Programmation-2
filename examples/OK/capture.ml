
let y = 4

(* let f x = x *)

let f x y = x y

let _ = f (fun x -> y) 3 (* should be 4 *)

(* )

   let x = 4 + 4

   let _ = (fun x -> 4) 3 *)