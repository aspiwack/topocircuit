open Latex
module Lex = Lexquot.Make (struct
  type t = MathParser.token
  type lexbuf = Ulexing.lexbuf

  let init = Ulexing.from_utf8_string

  let exhausted l =
    Ulexing.start l;
    let i = Ulexing.next l in
    if i = -1 then true
    else begin
      ignore (Ulexing.backtrack l);
      false
    end

  let lex_one = MathLexer.next
  let eos = MathParser.EOL

  let code x = MathParser.SYMB x
  let math x = MathParser.SYMB x
  let text x = MathParser.SYMB x
end)

let parse l =
  let parse =
    MenhirLib.Convert.traditional2revised
      (fun x -> x)
      (fun _ -> Lexing.dummy_pos)
      (fun _ -> Lexing.dummy_pos)
      MathParser.math
  in
  let next =
    let lexbuf = Lex.init l in
    fun () -> Lex.next lexbuf
  in
  parse next

let mathmode = parse
