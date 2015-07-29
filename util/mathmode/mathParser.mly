%{
 open Latex
 open Prelude


let empty_context = Latex.cdot

type simple_expr = {
  body : Latex.t;
  subscript : Latex.t option;
  superscript : Latex.t option;
}

let interp { body ; subscript ; superscript } =
  match subscript,superscript with
  | None,None -> body
  | Some sub , Some sup -> index_exponent body sub sup
  | Some sub, None -> index body sub
  | None , Some sup -> exponent body sup

let push_sub x = function
  | { subscript=None } as se -> { se with subscript = Some x }
  | se -> { body = interp se ; subscript=Some x; superscript=None }

let push_sup x = function
  | { superscript=None } as se -> { se with superscript = Some x }
  | se -> { body = interp se ; subscript=None; superscript=Some x }

let body x = { body=x; subscript=None; superscript=None }

%}

%token EOL
%token <Latex.t> SYMB

%token COMMA SEMICOLON COLON
%token METAPARENL METAPARENR
%token PARENL PARENR BRACKETL BRACKETR BRACEL BRACER
%token BBRACKETL BBRACKETR
%token WILDCARD
%token TURNSTYLE

%token POINTYL POINTYR
%token TIMES PLUS MINUS ARROW
%token PIPE

%token LAND LOR NEG IFF
%token FORALL EXISTS
%token UNION INTERSECTION

%token LAMBDA FAMILY COMPREHENSION MU
%token DOUBLEARROW
%token COMPOSITION SEQ

%token IN SUBSET CUP CAP
%token STAR
%token LEQ LT GEQ GT

%token EQUAL NEQUAL

%token SUB SUP

%token POWERSET
%token OPP

%start <Latex.t> math

%nonassoc LAMBDA
%right ARROW
%right PLUS
%right TIMES
%right APP
%nonassoc POWERSET
%nonassoc below_APP
%nonassoc STAR
%nonassoc NEG
%left SUB SUP

%%

math:
| e=expr EOL { mode M e }
| s=sequent EOL { mode M s }
| e=sequent_right_hand EOL { mode M e }


simple_expr_body:
| METAPARENL e=expr METAPARENR { e }
| s=SYMB { s }
| WILDCARD { text"\\_" }

| l=tuple(expr) { tuple l }
| POINTYL e=expr  POINTYR { between `Angle e }

| l=list(expr) { btuple l }
| BBRACKETL e=expr BBRACKETR { concat[llbracket;e;rrbracket] }
| BRACEL e=separated_nonempty_list (COMMA,expr) BRACER { between `Brace (concat_with_sep e (text",")) }

| NEG e=simple_expr { concat [ neg ; interp e ] }

simple_expr:
| e=simple_expr_body { body e }
| u=simple_expr s=post1 { s u }
| a=simple_expr s=op2s b=simple_expr { s a b }

expr:
| e=simple_expr %prec below_APP { interp e }
| t=expr u=spine %prec APP { t^^block (between `Paren (concat_with_sep u (text","))) }

| LAMBDA p=pattern COMMA e=expr {concat [lambda;p;text".\\,";e] } %prec LAMBDA

| s=pre1 u=simple_expr { s u }
//| s=pre2 u=simple_expr v=simple_expr { s (interp u) (interp v) }
| a=expr s=op2 b=expr { s a b }

| q=quantifier p=typedpattern COMMA b=expr { q p b }

appexpr:
| e=simple_expr %prec below_APP { interp e }

spine:
| u=appexpr  { [u] }
| s=spine u=appexpr { s@[u] }


%inline pre1:
| POWERSET { fun a -> powerset (interp a) }
| OPP { fun n -> text"-"^^(interp n) }

//%inline pre2:

%inline post1:
| STAR { fun a -> push_sup (text "*") a }

%inline op2:
| s=sop2 { fun a b -> concat[a;s;b] }

%inline sop2:
| TIMES { times }
| PLUS { text"+" }
| MINUS { text"-" }
| PIPE { text"~|~" }
| ARROW { rightarrow }
| DOUBLEARROW { rightarrow_ }
| IFF { iff }
| LAND { land_ }
| LOR { lor_ }
| IN { in_ }
| SUBSET { subseteq }
| EQUAL { text"=" }
| NEQUAL { neq }
| SEMICOLON { text";" }
| CUP { cup }
| CAP { cap }
| LEQ { leqslant }
| LT { text"<" }
| GEQ { geqslant }
| GT { text">" }
| COMPOSITION { circ }
| SEQ { cdot }

%inline op2s:
| SUB { fun a b -> push_sub (interp b) a }
| SUP { fun a b -> push_sup (interp b) a }

%inline quantifier:
| FORALL { forall }
| EXISTS { exists }
| UNION  { union  }
| INTERSECTION { intersection }
| FAMILY { family }
| COMPREHENSION { comprehension }
| MU { pfixedpoint }

%inline tuple(X):
  l=delimited(PARENL,separated_list(COMMA,X),PARENR) { l }

%inline list(X):
  l=delimited(BRACKETL,separated_list(COMMA,X),BRACKETR) { l }

typeannotation:
| COLON a=expr {a}

singletypedpattern:
| p = pattern a=option(typeannotation) { p , a }

typedpattern:
| p = nonempty_list(singletypedpattern) { p }

pattern:
| e=simple_expr { interp e }


nonempty_context:
| c=separated_nonempty_list(COMMA,context_item) {
  List.hd c ^^ Latex.concat (List.map (fun x -> text", "^^x) (List.tl c))
}
context:
| c=nonempty_context { c }
|   { empty_context }

context_item:
| p=singletypedpattern { default_interp_pattern p }

sequent_right_hand:
| e=expr { e }
| t=expr COLON a=expr { concat [ t ; text" : " ; a ] }

sequent_left_hand:
| g=context SEMICOLON gs=separated_nonempty_list(SEMICOLON,context) {
  g ^^ Latex.concat (List.map (fun x-> text"~;~"^^x) gs)
}
| c=nonempty_context { c }
| { Latex.empty }

sequent:
| l=sequent_left_hand TURNSTYLE r=sequent_right_hand {
  concat [ l ; vdash ; r ]
}
