open Ast

(* TODO:
   - handle param for let/let rec
   - Fix is not handle when alone.
   - Use boxes
*)

let print_binop fmt = function
  | Plus  -> Format.fprintf fmt "+"
  | Minus -> Format.fprintf fmt "-"
  | Times -> Format.fprintf fmt "*"
  | Div   -> Format.fprintf fmt "/"
  | And   -> Format.fprintf fmt "&&"
  | Or    -> Format.fprintf fmt "||"
  | Gt    -> Format.fprintf fmt ">"
  | Eq    -> Format.fprintf fmt "="

let print_loc fmt (l,c) =
  Format.fprintf fmt "(%d,%d)" l c

let print_loc_expr pp fmt (_,e) =
  Format.fprintf fmt "%a" pp e

let print_var    fmt = Format.fprintf fmt "%s"

let print_var_ty fmt = Format.fprintf fmt "%s"

let rec print_ty fmt = function
  | TyInt   -> Format.fprintf fmt "int"
  | TyBool  -> Format.fprintf fmt "bool"
  | TyVar x -> Format.fprintf fmt "%a" print_var_ty x
  | TyArrow(tyl,tyr) -> Format.fprintf fmt "%a -> %a" print_ty_wp tyl print_ty tyr
  | TyTimes(tyl,tyr) -> Format.fprintf fmt "%a * %a" print_ty_wp tyl print_ty tyr
and print_ty_wp fmt = function
  | TyInt | TyBool | TyVar _ as t -> print_ty fmt t
  | _ as ty -> Format.fprintf fmt "(%a)" print_ty ty


let print_either pp fmt = function
  | Left e  -> Format.fprintf fmt "fst %a" pp e
  | Right e -> Format.fprintf fmt "snd %a" pp e

let print_param fmt (var,mty) =
  match mty with
  | None    -> Format.fprintf fmt "%a" print_var var
  | Some ty -> Format.fprintf fmt "(%a:%a)" print_var var print_ty ty

let is_app (_,e) = match e with App _ -> true | _ -> false

let rec print_expr fmt expr =
  let print_lc_expr    = print_loc_expr print_expr    in
  let print_lc_expr_wp = print_loc_expr print_expr_wp in
  match expr with
  | Var v  -> Format.fprintf fmt "%a" print_var v
  | Int i  -> Format.fprintf fmt "%d" i
  | Bool b -> Format.fprintf fmt "%b" b
  | App(el,er) when is_app el -> Format.fprintf fmt "%a %a" print_lc_expr el print_lc_expr_wp er
  | App(el,er)                -> Format.fprintf fmt "%a %a" print_lc_expr_wp el print_lc_expr_wp er
  | Lam(var, mty, e)          -> Format.fprintf fmt "fun %a -> %a"
                                   print_param (var,mty) print_lc_expr e
  | Pair(el,er)               -> Format.fprintf fmt "(%a,%a)" print_lc_expr el print_lc_expr er
  | LetIn(var,(_,Fix(_,Lam(var',mty,e))),e') when var = var'
    -> Format.fprintf fmt "let rec %a = %a in %a" print_param (var',mty)
         print_lc_expr e print_lc_expr e'
  | Fix _                     -> assert false
  | LetIn(var,e,e')           -> Format.fprintf fmt "let %a = %a in %a" print_var var
                                   print_lc_expr e print_lc_expr e'
  | Proj(p)                   -> Format.fprintf fmt "%a" (print_either print_lc_expr) p
  | Ite(cond,el,er)           -> Format.fprintf fmt "if %a then %a else %a" (print_lc_expr) cond
                                   (print_lc_expr) el (print_lc_expr) er
  | Binop(b,el,er)            -> Format.fprintf fmt "%a %a %a" (print_lc_expr_wp) el
                                   print_binop b (print_lc_expr) er
and print_expr_wp fmt = function
  | Var _ | Int _ | Bool _ as e -> print_expr fmt e
  | _ as e -> Format.fprintf fmt "(%a)" print_expr e



let print_cmd fmt = function
  | Let(v,mty,e)    -> Format.fprintf fmt "let %a = %a" print_param (v,mty)
                         (print_loc_expr print_expr) e
  | LetRec(v,mty,e) -> Format.fprintf fmt "let rec %a = %a" print_param (v,mty)
                         (print_loc_expr print_expr) e

let print_ast fmt ast =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.@.")
    (print_loc_expr print_cmd) fmt ast;
  Format.fprintf fmt "@."
