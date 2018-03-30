%{
    open Ast

    let rec mk_lam ps t =
        match ps with
        | [] -> t
        | (x,None)::ps' -> fst x, Lam(snd x, None, mk_lam ps' t)
        | (x,Some ty)::ps' -> fst x, Lam(snd x, (Some ty), mk_lam ps' t)

%}

%token EOF
%token ARROW
%token COMMA COLON
%token LEFTPAR RIGHTPAR
%token EQUAL PLUS MINUS TIMES DIV AND OR
%token LET REC IN FUN
%token GT
%token INT BOOL
%token IF FST SND
%token THEN ELSE
%token<Utils.loc*string> ID
%token<string> TYID
%token<Utils.loc*int> NUM
%token<Utils.loc> TRUE FALSE

%left EQUAL
%left AND OR
%left GT
%left PLUS MINUS
%left TIMES DIV

%right ARROW

%start <Ast.t> main

%type <Utils.loc * expr> atomicexpr
%type <Utils.loc * expr> binopexpr
%type <Utils.loc * expr> appexpr
%type <Utils.loc * expr> funexpr
%type <Utils.loc * expr> letexpr
%type <ty> typ
%%

main: cmd* EOF
      { $1 }

typ:
    | INT
    { TyInt }
    | BOOL
    { TyBool }
    | tid=TYID
    { TyVar tid }
    | tl=typ ARROW tr=typ
    { TyArrow(tl,tr) }
    | tl=typ TIMES tr=typ
    { TyTimes(tl,tr) }

%inline binop:
    | PLUS
    { Plus }
    | MINUS
    { Minus }
    | TIMES
    { Times }
    | DIV
    { Div }
    | AND
    { And }
    | OR
    { Or }
    | EQUAL
    { Eq }
    | GT
    { Gt }

atomicexpr:
    | id=ID
       { fst id, Var (snd id) }
    | LEFTPAR e=letexpr RIGHTPAR
       { e }
    | LEFTPAR el=letexpr COMMA er=letexpr RIGHTPAR
      { fst el, Pair(el,er) }
    | n=NUM
        { fst n, Int(snd n) }
    | TRUE
        { $1, Bool true }
    | FALSE
        { $1, Bool false }

appexpr:
    | s=atomicexpr
        { s }
    | f=appexpr a=atomicexpr
        { fst f, App(f,a) }
    | FST e=atomicexpr
        { fst e, Proj(Left e) }
    | SND e=atomicexpr
        { fst e, Proj(Right e) }

binopexpr:
    | s=appexpr
        { s }
    | el=binopexpr b=binop er=binopexpr
        { fst el, Binop(b,el,er) }
funexpr:
    | s=binopexpr
        { s }
    | IF cond=funexpr THEN l=funexpr ELSE r=funexpr
        { fst cond, Ite(cond,l,r) }
    | FUN id=ID ARROW t=funexpr
        { fst id, Lam(snd id, None, t) }
    | FUN LEFTPAR id=ID COLON ty=typ RIGHTPAR ARROW t=funexpr
        { fst id, Lam(snd id, (Some ty), t) }

letexpr:
    | s=funexpr
        { s }
    | LET id=ID EQUAL t=letexpr IN u=letexpr
        { fst id, LetIn(snd id, t,u)  }
    | LET id=ID ps=param+ EQUAL t=letexpr IN u=letexpr
        { fst id, LetIn(snd id, mk_lam ps t,u)  }
    | LET REC id=ID EQUAL t=letexpr IN u=letexpr
        { fst id, LetIn(snd id, (fst t, Fix(fst id, Lam(snd id, None, t))), u) }
    | LET REC id=ID ps=param+ EQUAL t=letexpr IN u=letexpr
        { fst id, LetIn(snd id, (fst t, Fix(mk_lam ((id, None)::ps) t)), u) }


param:
    | id=ID
        {id, None}
    | LEFTPAR id=ID COLON ty=typ RIGHTPAR
        {id, Some ty}

cmd:
    | LET id=ID EQUAL t=letexpr
        { fst id, Let(snd id, None, t) }
    | LET id=ID ps=param+ EQUAL t=letexpr
        { fst id, Let(snd id, None, mk_lam ps t) }
    | LET REC id=ID ps=param+ EQUAL t=letexpr
        { fst id, LetRec(snd id, None, mk_lam ps t) }