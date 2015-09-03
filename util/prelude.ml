open Latex


let nabla = command "nabla" [] M
let implies = command "implies" ~packages:["amsmath",""] [] M
let log = command "log" [] M

let linebreak () = command "linebreak" [] A
let vspace_ l =
  let l = latex_of_size l in
  command "vspace" [T,l] T
let displaystyle x = block begin
  unusual_command "displaystyle" [M,nobr,x] M
end
let vdots = command "vdots" [] A
(* Some fonts do not have text figures built in; the textcomp package attempts
to remedy this by effectively generating text figures from the currently-selected
font. Put \usepackage{textcomp} in your preamble. textcomp also allows you to
use decimal points, properly formatted dollar signs, etc. within \oldstylenums{}.
  *)
let oldstylenums x= command "oldstylenums" [T,x] T
let pageref (t:label) = command "pageref" [A,text (Obj.magic t)] A
let relax = command "relax" [] A
(* requires package bookmark, needs reflexion sur les options des
   packages requirés ! *)
(* on pourrait faire un rich type des options ! *)
let bookmarksetup x = command "bookmarksetup" [T,x] T
let ae = command "ae" [] T
let xrightarrow f = command ~packages:["amsmath",""] "xrightarrow" [M,f] M
let xRightarrow f = command ~packages:["mathtools",""] "xRightarrow" [M,f] M
let partial = command "partial" [] M
let blacklozenge = command "blacklozenge" ~packages:["amssymb",""] [] M
let blacksquare = command "blacksquare" ~packages:["amssymb",""] [] M
let centerdot = command "centerdot" ~packages:["amssymb",""] [] M
  (* arnaud: le package mathabx semble conflicter *)
let blackdiamond = command "blackdiamond" ~packages:["mathabx",""] [] M
let sqbullet = command "sqbullet" ~packages:["mathabx",""] [] M
let thinspace = text"\\,"
let sffamily x = block(unusual_command "sffamily" [T,nobr,x] T)
let itshape x = block(unusual_command "itshape" [T,nobr,x] T)
let upalpha = command "upalpha" ~packages:["upgreek",""] [] M
let upbeta = command "upbeta" ~packages:["upgreek",""] [] M
let upgamma = command "upgamma" ~packages:["upgreek",""] [] M
let updelta = command "updelta" ~packages:["upgreek",""] [] M
let upepsilon =  command "upepsilon" ~packages:["upgreek",""] [] M
let upeta = command "upeta" ~packages:["upgreek",""] [] M
let upiota = command "upiota" ~packages:["upgreek",""] [] M
let upmu = command "upmu" ~packages:["upgreek",""] [] M
let uppi = command "uppi" ~packages:["upgreek",""] [] M
let uprho = command "uprho" ~packages:["upgreek",""] [] M
let upsigma = command "upsigma" ~packages:["upgreek",""] [] M
let upomega = command "upomega" ~packages:["upgreek",""] [] M
  (* le package pxfonts semble conflicter *)
let alphaup = command "alphaup" ~packages:["pxfonts",""] [] M
let rightarrowtail = command "rightarrowtail" ~packages:["amssymb",""] [] M
let appendixpage = text"\\appendixpage\n"
let appendix = text"\\appendix\n"

(* Couche autour de \color dans latex. *)
(* arnaud: pour rgb, cf http://en.wikibooks.org/wiki/LaTeX/Colors *)
module RgbSet = Map.Make (struct type t = float*float*float let compare = compare end)
let rgbcolors = variable ~eq:(RgbSet.equal (=)) RgbSet.empty
let rgbgensym = variable 0
type color = Latex.t
  let white = text"white"
  let black = text"black"
  let red = text"red"
  let green = text"green"
  let blue = text"blue"
  let magenta = text"magenta"
  let yellow = text"yellow"
  let rgb r g b =
    get rgbcolors (fun s ->
      try
	let name = RgbSet.find (r,g,b) s in
	text name
      with Not_found ->
	get rgbgensym (fun n ->
          let name = "melt-rgb-color-"^(string_of_int n) in
	  set rgbcolors (RgbSet.add (r,g,b) name s)^^
          incr_var rgbgensym^^
          text name)
    )

let color_prelude =
  final rgbcolors begin fun s ->
    RgbSet.fold begin fun (r,g,b) name acc ->
      let rgb = concat [
	latex_of_float r; text",";
	latex_of_float g; text",";
	latex_of_float b]
      in
      command "definecolor" [ A,text name ; A,text"rgb" ; A,rgb ] A
    end s Latex.empty
  end

let color =
  fun c t -> block begin
    unusual_command "color" ~packages:["color",""]
      [A,brace,c ; A,nobr,t] A
  end

let nothing = within_braces empty
let qed = square
let textnums = oldstylenums
let bookmarkalt ~latex ~bookmark =
  command "texorpdfstring" [T,latex ; T,bookmark] T

let mlplatex = Melt.picture_of_latex

let within_parentheses t = concat [ text"(" ; t ; text ")" ]

let vspace_star s =
  command "vspace*" [T,latex_of_size s] T
let setlength c s t = within_braces begin
  unusual_command "setlength" [
    T , nobr , (command c [] T) ;
    T , brace , (latex_of_size s) ;
    T , nobr , t
  ] T
end


let set_lineskip s t = setlength "baselineskip" s (concat[t;par])


let concat_with_sep l sep =
  if l = [] then
    concat []
  else
    let l_with_one_too_many = List.flatten (List.map (fun t -> [ sep ; t ]) l) in
    concat (List.tl l_with_one_too_many)

(* attention, tel quel ça utilise memoir. Pour transvaser dans
   latex.ml, il faudra distribuer [bullet] sur chaque [\item] et abandonner
   spacing…*)
let itemize ?(spacing=`Default) ?bullet items =
  let spacing = match spacing with
    | `Default -> empty
    | `Tight -> text"\\tightlist"
    | `Firm -> text"\\firmlist"
  in
  if items = [] then failwith "itemize needz itemz";
  let items = List.map ((^^) (text "\\item ")) items in
  let body = concat_with_sep items (text "\n") in
  let opt = Option.map (fun b -> T,b) bullet in
  environment "itemize" ?opt (T, spacing^^body) T

(* changes default from itemize *)
let itemize ?spacing ?(bullet=(tiny blacksquare)) items =
  itemize ?spacing ~bullet items



type pattern = Latex.t*(Latex.t option)

let interp_pattern_autospacing (x,a) =
  let in_a = Option.map (fun x -> in_^^x) a in
  concat @@ Option.flatten [ Some x ; in_a ]
let interp_pattern_tightspacing (x,a) =
  let space = text"\\," in
  let in_a =
    Option.map
      (fun x -> concat [ space ; block in_ ; space ; x ])
      a
  in
  concat @@ Option.flatten [ Some x ; in_a ]
let interp_pattern_sup (x,a) =
  match a with
  | None -> x
  | Some a -> exponent x (in_^^a)
let interp_pattern_small (x,a) =
  let space = text"\\," in
  let in_a =
    Option.map
      (fun  x -> scriptsize (concat [ block in_ ; space ; x ]))
      a
  in
  concat @@ Option.flatten [ Some x ; in_a ] 

let default_interp_pattern = interp_pattern_autospacing


let quantifier c q t =
  mode M (concat [ c ; text"_" ; scriptsize (interp_pattern_autospacing q) ; t ])
let quantifier c = List.fold_right (quantifier c)
let union = quantifier bigcup
let intersection = quantifier bigcap

(* let hquantifier c q t = *)
(*   mode M (concat [ c ; (interp_pattern_sup q) ; text ".\\," ; t ]) *)
(* let hquantifier c = List.fold_right (hquantifier c) *)
let hquantifier c q t =
  let q = concat_with_sep (List.map interp_pattern_sup q) (text",") in
  mode M (concat [ c ; q ; text ".\\,\\," ; t ])
let forall = hquantifier forall
let exists = hquantifier exists

let family q t =
  index
    (between `Paren t)
    (interp_pattern_autospacing q)
let family = List.fold_right family

let comprehension p b = between `Brace (concat [ interp_pattern_tightspacing p ; mid ; b])
let comprehension p b =
  match p with
  | [p] -> comprehension p b
  | _ -> failwith "comprehension cannot be used with several patterns."

let pfixedpoint p b =
  let rec split = function
    | [] -> assert false
    | [x] -> [] , x
    | a::p ->
        let (b,x) = split p in
        a::b,x
  in
  let (a,x) = split p in
  let params =
    a |> List.map interp_pattern_small
      |> fun l -> concat_with_sep l (text",")
  in
  mode M @@
    concat [index mu (within_braces params);
            interp_pattern_sup x; text ".\\,\\," ;
            b ]

let powerset u =
  concat [
    mathcal (text "P");
    block (between `Paren u)
  ]

(*** An draft of module for the [memoir] document class ***)

(* arnaud: options comme [Latex.t]. Faire quelque chose de plus exhaustif
      avant d'en faire un morceau de package officiel. *)
let document ?(options=[]) ?(prelude=empty) ?(packages=[]) body =
  let options = (T,concat_with_sep options (text",")) in
  concat [
    documentclass ~opt:options (text"memoir"); par;
    require_packages packages;
    required_packages; par;
    prelude; par;
    documentmatter body;
  ]

let titlingpage x = environment "titlingpage" (T,x) T


let epigraph txt src = command "epigraph" [(T,txt);(T,src)] T

let epigraphhead ?dist x =
  let opt = Option.map (fun d->T,latex_of_size d) dist in
  command "epigraphhead" ?opt [T,x] T


let midrule = array_command (text"\\midrule")
let bottomrule = array_command (text"\\bottomrule")
let cmidrule (trim,s,t)=
  let trim = match trim with
    | `R -> text"r"
    | `L -> text"l"
    | `LR -> text"lr"
    | `RL -> text"rl"
  in
  let trim = within_parentheses trim in
  let range = block (
    latex_of_int s ^^
    text"-" ^^
    latex_of_int t)
  in
  text "\\cmidrule"^^trim^^range
let cmidrules l =
  array_command (concat (List.map cmidrule l))

let spacing f x =
  environment "Spacing" ~args:[A,latex_of_float f] (T,x) T


(*** /memoir ***)


let displaymode = variable false

let scoped_set v x t =
  get v begin fun old ->
   set v x ^^ t ^^ set v old
  end

let display_mode x =
  scoped_set displaymode true x

let displaybox ?(centered = false) x =
  let inner =
    if centered then
      x
    else
      makebox (`Linewidth 1.) (
	makebox (`Linewidth 0.1) empty ^^
	  parbox (`Linewidth 0.9) x
      )
  in
  displaymath (display_mode inner)

(*
concat [
  par ;
  text "\\nointerlineskip" ; makebox (`Linewidth 0.6) empty ;
  parbox (`Linewidth 0.6) x ;
(*  par ; *)
  text "\\ignorespaces" ;
(*  text "\\@ignoretrue" *)
]*)

let includegraphics ?height ?width t =
  let param k n x =
    Option.map (fun x -> concat [text n;text"=";k x]) x
  in
  let sparam = param latex_of_size in
  let width = sparam "width" width in
  let height = sparam "height" height in
  let opt = A,concat_with_sep (Option.flatten [height;width])  (text",") in
  command "includegraphics" ~packages:["graphicx",""] ~opt [A,t] A

let vbox x =
  command "vbox" [A,x] A
let hbox x =
  command "hbox" [A,x] A

(* arnaud: je reprends un modèle plus simple pour l'instant
let quote ?from t =
  let openg = includegraphics ~height:(`Mm 5.) ~width:(`Mm 7.14) (text"guillemet1.png") in
  let closeg = includegraphics ~height:(`Mm 5.) ~width:(`Mm 7.14) (text"guillemet2.png") in
    (*let closeg = vbox (hfill^^vspace (`Stretch 1) ^^ closeg) in*)
    (*let openg = vbox (*((*makebox (`Mm 7.14)*) openg)*) (text"A") in*)
  let closeg = raisebox ~lift:(`Ex (-1.)) closeg in
    (*let t = vbox (parbox (`Textwidth 0.7) t) in*)
  let q = concat [ openg ; t ; (*hspace (`Stretch 1);*) closeg ] in
  (*let t = parbox (`Textwidth 0.7) t in
   let q = array [`L;`L;`L] [ array_line [ openg; t ;closeg ] ] in*)
    (*let q = hbox q in*)
  let f = match from with
    | None -> empty
    | Some from -> text"\\\\-- "^^from
  in
  let t = noindent^^q^^f in
  quotation t
    (*parbox (`Textwidth 0.6) t*)
  *)
let quote ?from t =
  (* arnaud: pas de gestion du from *)
  quotation t

(* arnaud:
(* trick to allow the use of a "quote" label in the [chapter] command. *)
let _quote = quote
  *)
(* Book chapter with quote *)

(* arnaud
let flushleftright x= block begin
  unusual_command "flushleftright" [A,nobr,x] A
end
  *)

let foreign = emph

let booktitle = emph

(*
let raggedleft t = command "raggedleft" [A,t] A
let raggedright t = command "raggedright" [A,t] A
let oldmarginpar o t = command "marginpar" ~opt:(A,o) [T,t] A
  *)
let marginpar t =
  command "marginpar" [T,
     (footnotesize (set_lineskip (`Baselineskip 1.)  t))
  ] A
(*
  unusual_command "-" [A,nobr,oldmarginpar
			 (raggedleft(footnotesize t))
			 (raggedright(footnotesize t))
  ] A
*)


let parnopar = text "\\parnopar "

let marginal_note ?(shift=0.) t =
  let offset = 5. +. shift in
  let unit s = `Mm s in
  text"\\strictpagecheck "^^
  marginpar (concat[vspace_star (unit (-.offset));t;vspace_star (unit (offset))])
let _proof = marginal_note

let fact ~proof t =
  concat [ _proof proof ; t ]

let note ?shift x ~note = marginal_note ?shift note ^^ x

let ie = foreign (mode T (text "i.e."))

let ph =
  let spacing =
    text"\\,"
  in
  concat [
    spacing;
    rule_ ~lift:(`Ex 0.5) (`Em 0.25) (`Pt 0.3);
    spacing
  ]
let noname = mode M (text "\\_")
let powerset e = concat [ block (mathcal (text "P")) ;  e ]
let leq = leqslant
let geq = geqslant
let phi = varphi

(* arnaud: penser à voir si on peut faire le \input automatiquement basé
   sur la syntaxe du second argument *)
let initfamily =
  command "usefont" [(A,text"U");(A,text"RoyalIn");(A,text"xl");(A,text"n")] A

let lettrine ?(nlines=4) ?(loversize=0.2) ?(lhang=0.) ?(nindent=`Em 0.)l m =
  let nlines = text "lines="^^(latex_of_int nlines) in
  let loversize = text "loversize="^^(latex_of_float loversize) in
  let nindent = text "nindent="^^(latex_of_size nindent) in
  let lhang = text "lhang="^^(latex_of_float lhang) in
  let opt = (A,concat_with_sep [
	       nlines;loversize;nindent;lhang
	     ] (text","))
  in
  command "lettrine" ~opt ~packages:["lettrine",""] [(T,initfamily^^l);(T,m)] T

let courier x =
  let uf = command "usefont" [(A,text"T1");(A,text"pcr");(A,text"m");(A,text"n")] A in
  block (uf^^x)
let avantgarde x =
  let uf = command "usefont" [(A,text"T1");(A,text"pag");(A,text"m");(A,text"n")] A in
  block (uf^^(small x))
let avantgarde_bold x =
  let uf = command "usefont" [(A,text"T1");(A,text"pag");(A,text"b");(A,text"n")] A in
  block (uf^^(small x))
(* usefont encoding fontname series shape:
		       encoding : U=unknown ;  T1 = TeX extended text (probablement postscript type 1)
		       series: m=medium ; b=bold ; bx=bold extended(black?) ; sb=semi-bold ; c=condensed ; xl=??
		       shape: n=normal ; it=italique ; sl=slanted ; sc=small caps
    *)

(* extra commands *)
let quotient a b = (*mode M (concat [ a ; text "/" ; b ])*)
  command ~packages:["faktor",""] "faktor" [(M,a);(M,b)] M

let tuple l =
  mode M (between `Paren (concat_with_sep l (text ",")))

let atuple l =
  mode M (between `Angle (concat_with_sep l (text ",")))

let btuple l =
  mode M (between `Bracket (concat_with_sep l (text ",")))

(*** Bindings to theorem environment ***)


let newtheorem cmd name =
    command "newtheorem" [(T,cmd);(T,name)] T

let theorems = variable []

(* arnaud: le problème ici c'est que "name" est un Latex.t, donc peut contenir des fonctions. C'est assez dangereux. *)
let declare_theorems =
  final theorems begin fun l ->
    concat (List.map (fun (c,n) -> newtheorem (text c) n) l)
  end

let require_theorem =
  let gensym = variable 0 in
  fun x k ->
    setf gensym ((+)1) ^^
    get gensym begin fun g -> 
      let name = "thm__"^string_of_int g in
      setf theorems (fun l -> (name,x)::l)^^
      k name
    end

let theorem_env name ?label ?caption body =
  let label_and x =
    match label with
    | None -> x
    | Some l -> place_label l ^^ x
  in
  let opt = Option.map (fun x -> T,x) caption in
  environment name ?opt (T,label_and body) T

let make_theorem name =
  let exists = variable None in
  fun ?label ?caption t ->
    get exists begin function
      | None ->
          require_theorem name (fun cmd -> set exists (Some cmd) ^^ theorem_env cmd ?label ?caption t)
      | Some cmd -> theorem_env cmd ?label ?caption t
    end

let proof_env t =
  environment "proof" ~packages:["amsthm",""] (T,t) T

let theorem_bare = make_theorem (text"Theorem")
let theorem ?label ?name ?proof statement =
  concat [
    theorem_bare ?label ?caption:name statement;
    begin match proof with
    | None -> empty
    | Some p -> proof_env p
    end
  ]

let lemma_bare = make_theorem (text"Lemma")
let lemma ?label ?name ?proof statement =
  concat [
    lemma_bare ?label ?caption:name statement;
    begin match proof with
    | None -> empty
    | Some p -> proof_env p
    end
  ]

let definition_bare = make_theorem (text"Definition")
let definition ?label ?name def =
  definition_bare ?label ?caption:name def

let remark_bare = make_theorem (text"Remark")
let remark ?label ?name def =
  remark_bare ?label ?caption:name def

(*** Labels ***)

(**** Bibliography ****)
let cite ?extra t =
  let opt = Option.map (fun x -> (T,x)) extra in
  command "cite" ?opt [T,t] T
let page i =
  text"p.~"^^latex_of_int i
let pages s e =
  text"p.~"^^latex_of_int s^^text"--"^^latex_of_int e
