
open Ast

exception Typing_error of label*string

exception Internal_error of string

let make_label startpos endpos =
    let sp = startpos.Lexing.pos_bol in
         {file=(startpos.Lexing.pos_fname);
          line=(startpos.Lexing.pos_lnum);
          cbegin=(startpos.Lexing.pos_cnum - sp);
          cend=(endpos.Lexing.pos_cnum - sp)} 

let string_of_label lb =
    let cend = (if lb.cbegin < lb.cend then lb.cend else lb.cbegin+1) in
        Format.sprintf "File %s, line %d, characters %d-%d:\n"
        lb.file
        lb.line
        lb.cbegin
        cend




