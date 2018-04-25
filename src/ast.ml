type binop = Plus | Minus | Times | Div | And | Or | Eq | Gt

type 'a loc = Utils.loc * 'a

type var = string

type var_ty = string

type ty =
  | TyInt
  | TyBool
  | TyVar of var_ty
  | TyArrow of ty * ty
  | TyTimes of ty * ty
  | TyUnit

type 'a either = Left of 'a | Right of 'a

type expr =
  | Var   of var
  | App   of expr loc * expr loc
  | Lam   of var * ty option * expr loc
  | Pair  of expr loc * expr loc
  | LetIn of var * expr loc * expr loc
  | Fix   of expr loc
  | Int   of int
  | Bool  of bool
  | Proj  of expr loc either
  | Ite   of expr loc * expr loc * expr loc
  | Binop of binop * expr loc * expr loc
  | Unit


type cmd =
  | Let  of var * ty option * expr loc
  | LetRec of var * ty option * expr loc

type t = cmd loc list
