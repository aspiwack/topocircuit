open Latex

val nabla : Latex.t
val implies : Latex.t
val log : t
(* [linebreak] avant de la commiter dans melt, il faudra penser à vérifier sa sémantique *)
val linebreak : unit -> t
val vspace_ : size -> t
val vdots : t
val displaystyle : t -> t
val oldstylenums : t -> t
val relax : t
val pageref : label -> t
val bookmarksetup : t -> t
val ae : t
val xrightarrow : t -> t
val xRightarrow : t -> t
val partial : t
val blacklozenge : t
val itemize : ?spacing:[`Default|`Firm|`Tight] -> ?bullet:t -> t list -> t
val blacksquare : t
val centerdot : t
(*val blackdiamond : t
  val sqbullet : t*)
val thinspace : t
val sffamily : t -> t
val itshape : t -> t
val upalpha : t
val upbeta : t
val upgamma : t
val updelta : t
val upepsilon : t
val upeta : t
val upiota : t
val upmu : t
val uppi : t
val uprho : t
val upsigma : t
val upomega : Latex.t
  (*val alphaup : t*)
val rightarrowtail : t
val appendixpage : t
  (* fix de la commande dans Melt *)
val appendix : t

type color
  val white : color
  val black : color
  val red : color
  val green : color
  val blue : color
  val magenta : color
  val yellow : color
  val rgb : float -> float -> float -> color
val color : color -> t -> t
val color_prelude : t

val nothing : t
val qed : t
val textnums : t -> t
val bookmarkalt : latex:t -> bookmark:t -> t

val parnopar : t
val note : ?shift:float -> t -> note:t -> t

val concat_with_sep : t list -> t -> t

val mlplatex : t -> Mlpost.Picture.t

val within_parentheses : t -> t


type pattern = Latex.t*Latex.t
val default_interp_pattern : pattern -> Latex.t

val forall : pattern list -> Latex.t -> Latex.t
val exists : pattern list -> Latex.t -> Latex.t
val union  : pattern list -> Latex.t -> Latex.t
val intersection : pattern list -> Latex.t -> Latex.t
val family : pattern list -> Latex.t -> Latex.t
val comprehension : pattern list -> Latex.t -> Latex.t

val powerset : Latex.t -> Latex.t

(*** Definitions/theorems ***)


val declare_theorems : Latex.t
val theorem : ?label:Latex.label -> ?name:Latex.t -> ?proof:Latex.t -> Latex.t -> Latex.t
val definition : ?label:Latex.label -> ?name:Latex.t -> Latex.t -> Latex.t

(*** An draft of module for the [memoir] document class ***)

val document : ?options:t list -> ?prelude:t -> ?packages:(t*t) list -> t -> t

val titlingpage : t -> t


val epigraph : t -> t -> t
val epigraphhead : ?dist:size -> t -> t

val midrule : array_line
val bottomrule : array_line
val cmidrule : [`R|`L|`RL|`LR]*int*int -> Latex.t (* Low level *)
val cmidrules : ([`R|`L|`RL|`LR]*int*int) list -> array_line

val spacing : float -> t -> t

(*** /memoir ***)

val courier : t -> t
val avantgarde : t -> t
val avantgarde_bold : t -> t

val lettrine :
  ?nlines:int ->
  ?loversize:float ->
  ?lhang:float ->
  ?nindent:size ->
  t -> t -> t

val quote : ?from:t -> t -> t

(* Box to print math and the like *)
val displaybox : ?centered:bool -> t -> t
val display_mode : t -> t

val foreign : t -> t
val booktitle : t -> t

val marginpar : t -> t
(* inlined fact with a proof *)
val fact : proof:t -> t -> t

val ie : t

(* Argument placeholder for misfix functions *)
val ph : t
(* Wilcard name for binders and definitions *)
val noname : t

val powerset : t -> t
(* less or equal / greater or equal *)
val leq : t
val geq : t

val phi : t

(* extra commands *)
val quotient : t -> t -> t
val tuple : t list -> t
val atuple : t list -> t
val btuple : t list -> t

(*** Labels ***)

(**** Bibliography ****)
val cite : ?extra:t -> t -> t
val page : int -> t
val pages : int -> int -> t
