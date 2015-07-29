open MathParser
open Latex
open Prelude

(*** Formatting primitive ***)

let keyword k = SYMB (textbf (text k))
let longvar x = SYMB (mathsf (text x))
let number n = SYMB (mode M (text n))
let calvar x = SYMB (mathcal (text x))
let bbvar x = SYMB (mathbb (text x))

(*** Global tables ***)

let keywords = []

let map_ident = [
  "lambda" , LAMBDA ;
  "MU" , MU ;
  "family" , FAMILY ;
  "compr" , COMPREHENSION ;

  "ph" , SYMB cdot ;

  "COMP" , COMPOSITION ;

  "Top" , SYMB top ;
  "Bot" , SYMB bot ;
  "forall", FORALL ;
  "exists", EXISTS ;
  "Union" , UNION ;
  "Inter" , INTERSECTION ;

  "IN"  , IN ;
  "SUB" , SUBSET ;
  "cup" , CUP ;
  "cap" , CAP ;

  "LEQ" , LEQ ;
  "LT", LT ;
  "GEQ" , GEQ ;
  "GT" , GT ;

  "plus" , SYMB(text"+") ;
  "minus" , SYMB(text"-") ;
  "zero" , SYMB(text"0") ;

  "opp" , OPP ;

  "emptyact" , SYMB cdot;

  (*greek letters*)
  "alpha" , SYMB alpha ;
  "beta" , SYMB beta ;
  "Gamma" , SYMB gamma_;
  "gamma" , SYMB gamma ;
  "delta" , SYMB delta;
  "Delta" , SYMB delta_;
  "epsilon" , SYMB varepsilon;
  "iota" , SYMB iota ;
  "kappa" , SYMB kappa ;
  "lam" , SYMB lambda ;
  "mu" , SYMB mu ;
  "Pi" , SYMB pi_;
  "pi" , SYMB pi ;
  "Sigma" , SYMB sigma_;
  "sigma" , SYMB sigma ;
  "phi" , SYMB varphi ;
  "Phi", SYMB phi_;
  "Xi" , SYMB xi_;
  "Psi" , SYMB psi_;
  "omega" , SYMB omega ;
  "Omega" , SYMB omega_;
]

let regexp whitespace = [ ' '  '\t' '\n' '\r' ]

let regexp lower_case = ['a'-'z']
let regexp upper_case = ['A'-'Z']
let regexp letter = (lower_case | upper_case)
let regexp digit = ['0'-'9']

let regexp lvar = lower_case['\'']*
let regexp uvar = upper_case['\'']*
let regexp cal_var = 'c' upper_case
let regexp bold_var = 'b' letter
let regexp bb_var = "bb" upper_case
let regexp var_star = upper_case "_*"
let regexp ident = letter+
let regexp number = "-"? digit+

let lexeme = Ulexing.utf8_lexeme


let map =
  map_ident @
  (List.map (fun k -> k,keyword k) keywords)

let next k = lexer
  (* arnaud: je l'ai mis en haut pour ne pas interférer avec [ident]. Néanmoins, l'objectif est plutôt d'utiliser l'unicode, mais ça ne marche pas out of the box apparemment. *)
  | "sP" | "℘" -> POWERSET

  | "r0" -> SYMB (mathbf(text"0"))
  | "r1" -> SYMB (mathbf(text"1"))

  | lvar | uvar -> SYMB (mode M (text (lexeme lexbuf)))
  | cal_var ->
      let pre = lexeme lexbuf in
      let l = String.length pre - 1 in
      (* removes the 'c' in front *)
      calvar (String.sub pre 1 l)
  | bb_var ->
      let pre = lexeme lexbuf in
      let l = String.length pre - 2 in
      (* removes "bb" in front *)
      bbvar (String.sub pre 2 l)

  | ident ->
    let id = lexeme lexbuf in
    begin try
	 List.assoc id map
      with Not_found ->
	longvar id
    end

  | "<" | "⟨" -> POINTYL | ">" | "⟩" -> POINTYR

  | "\\(" -> METAPARENL | "\\)" -> METAPARENR

  | "|-" | "⊢" -> TURNSTYLE

  | "," -> COMMA | ";" -> SEMICOLON
  | ":" -> COLON
  | "." -> SEQ
  | "|" -> PIPE
  | "(" -> PARENL | ")" -> PARENR
  | "[" -> BRACKETL | "]" -> BRACKETR
  | "[[" -> BBRACKETL | "]]" -> BBRACKETR
  | "{" -> BRACEL | "}" -> BRACER
  | "_" -> SUB | "^" -> SUP

  | "+" -> PLUS | "*" | "×" -> TIMES
  | "-" -> MINUS

  | "^*" -> STAR

  | "->" | "→" -> ARROW
  | "\\/" | "∨" -> LOR | "/\\" | "∧"-> LAND
  | "~" -> NEG

  | "=>" | "⇒" -> DOUBLEARROW
  | "<=>" -> IFF

  | "\\_" -> WILDCARD

  | "=" -> EQUAL | "<>" -> NEQUAL

  | whitespace -> k ()
  | number -> number (lexeme lexbuf)
