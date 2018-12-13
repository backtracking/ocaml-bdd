(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(**************************************************************************)

{
  open Lexing
  open Parser

  exception Lexical_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum;
        pos_cnum=0 }

}

let space = [' ' '\t' '\r']+
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' ':' '_']*
let number = ['0' - '9']+

rule token = parse
  | '\n'
      { newline lexbuf; token lexbuf }
  | space
      { token lexbuf }
  | '#' [^'\n']* ('\n' | eof)
      { newline lexbuf; token lexbuf }
  | '('
      { LPAR }
  | ')'
      { RPAR }
  | '~'
      { NOT }
  | "->"
      { IMP }
  | "<->"
      { EQUIV }
  | "/\\" | '&'
      { AND }
  | "\\/" | 'v'
      { OR }
  | ident
      { IDENT (lexeme lexbuf) }
  | _
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }
  | eof
      { EOF }

{
  let formula_of_string s =
    let lb = Lexing.from_string s in
    match Parser.file token lb with
    | f :: _ -> f
    | _ -> assert false
}

