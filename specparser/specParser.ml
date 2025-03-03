
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | UNION
    | UINST
    | TYPE
    | TRUE
    | SUBSETEQ
    | SUBSET
    | STRCONST of (
# 65 "specParser.mly"
       (string)
# 21 "specParser.ml"
  )
    | STEXC
    | STATE
    | STAR
    | SEMICOLON
    | RPAREN
    | RELATION
    | REF
    | RCURLY
    | RBRACE
    | QUAL
    | PURE
    | PRIMITIVE
    | PLUS
    | PIPE
    | OF
    | NUMEQ
    | NOT
    | MINUS
    | LPAREN
    | LESSTHAN
    | LCURLY
    | LBRACE
    | LAMBDA
    | INT of (
# 64 "specParser.mly"
       (int)
# 49 "specParser.ml"
  )
    | IMPL
    | IFF
    | ID of (
# 63 "specParser.mly"
        (string)
# 56 "specParser.ml"
  )
    | GREATERTHAN
    | FORMULA
    | FALSE
    | EXISTS
    | EXC
    | EQUALOP
    | EOL
    | EOF
    | DOT
    | DISJ
    | CROSSPRD
    | CONJ
    | COMMA
    | COLONARROW
    | COLON
    | ASSUME
    | ARROW
    | ARMINUS
    | ANGRIGHT
    | ANGLEFT
    | ALL
  
end

include MenhirBasics

# 1 "specParser.mly"
  
open SpecLang
open RelLang
open Printf
module TypeSpec = SpecLang.RelSpec.TypeSpec
module RefTy = SpecLang.RefinementType
module Formula = SpecLang.RelSpec.Formula
module Qualifier = SpecLang.Qualifier
exception Error
let defaultCons = SpecLang.Tycon.default
let symbase = "sp_"
let count = ref 0
let genVar = fun _ -> 
  let id = symbase ^ (string_of_int (!count)) in 
  let () = count := !count + 1
    in
      Var.fromString id 
let ($) (f,arg) = f arg
let  empty = fun _ -> Vector.new0 ()
let print msg = let () = Printf.printf "%s" msg in ()


# 107 "specParser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_start) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: start. *)

  | MenhirState003 : (('s, _menhir_box_start) _menhir_cell1_TYPE _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 003.
        Stack shape : TYPE ID.
        Start symbol: start. *)

  | MenhirState005 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 005.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState007 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 007.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState012 : (('s, _menhir_box_start) _menhir_cell1_tpatmatch, _menhir_box_start) _menhir_state
    (** State 012.
        Stack shape : tpatmatch.
        Start symbol: start. *)

  | MenhirState016 : (('s, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 016.
        Stack shape : RELATION ID.
        Start symbol: start. *)

  | MenhirState017 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 017.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState020 : ((('s, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_params, _menhir_box_start) _menhir_state
    (** State 020.
        Stack shape : RELATION ID params.
        Start symbol: start. *)

  | MenhirState023 : (('s, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 023.
        Stack shape : LPAREN ID.
        Start symbol: start. *)

  | MenhirState025 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 025.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState033 : (('s, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_conpat, _menhir_box_start) _menhir_state
    (** State 033.
        Stack shape : LPAREN conpat.
        Start symbol: start. *)

  | MenhirState035 : (('s, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_state
    (** State 035.
        Stack shape : LPAREN.
        Start symbol: start. *)

  | MenhirState037 : (('s, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_state
    (** State 037.
        Stack shape : LCURLY.
        Start symbol: start. *)

  | MenhirState047 : (('s, _menhir_box_start) _menhir_cell1_elem, _menhir_box_start) _menhir_state
    (** State 047.
        Stack shape : elem.
        Start symbol: start. *)

  | MenhirState049 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 049.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState050 : (('s, _menhir_box_start) _menhir_cell1_LBRACE, _menhir_box_start) _menhir_state
    (** State 050.
        Stack shape : LBRACE.
        Start symbol: start. *)

  | MenhirState051 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 051.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState054 : ((('s, _menhir_box_start) _menhir_cell1_LBRACE, _menhir_box_start) _menhir_cell1_instexpr, _menhir_box_start) _menhir_state
    (** State 054.
        Stack shape : LBRACE instexpr.
        Start symbol: start. *)

  | MenhirState059 : (('s, _menhir_box_start) _menhir_cell1_ratom, _menhir_box_start) _menhir_state
    (** State 059.
        Stack shape : ratom.
        Start symbol: start. *)

  | MenhirState062 : (('s, _menhir_box_start) _menhir_cell1_instexpr, _menhir_box_start) _menhir_state
    (** State 062.
        Stack shape : instexpr.
        Start symbol: start. *)

  | MenhirState068 : (('s, _menhir_box_start) _menhir_cell1_funparam, _menhir_box_start) _menhir_state
    (** State 068.
        Stack shape : funparam.
        Start symbol: start. *)

  | MenhirState072 : (('s, _menhir_box_start) _menhir_cell1_ratom, _menhir_box_start) _menhir_state
    (** State 072.
        Stack shape : ratom.
        Start symbol: start. *)

  | MenhirState074 : (('s, _menhir_box_start) _menhir_cell1_ratom, _menhir_box_start) _menhir_state
    (** State 074.
        Stack shape : ratom.
        Start symbol: start. *)

  | MenhirState076 : (('s, _menhir_box_start) _menhir_cell1_ratom, _menhir_box_start) _menhir_state
    (** State 076.
        Stack shape : ratom.
        Start symbol: start. *)

  | MenhirState078 : (('s, _menhir_box_start) _menhir_cell1_ratom, _menhir_box_start) _menhir_state
    (** State 078.
        Stack shape : ratom.
        Start symbol: start. *)

  | MenhirState084 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 084.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState086 : (((('s, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_params, _menhir_box_start) _menhir_cell1_EQUALOP, _menhir_box_start) _menhir_state
    (** State 086.
        Stack shape : RELATION ID params EQUALOP.
        Start symbol: start. *)

  | MenhirState091 : (('s, _menhir_box_start) _menhir_cell1_patmatch, _menhir_box_start) _menhir_state
    (** State 091.
        Stack shape : patmatch.
        Start symbol: start. *)

  | MenhirState093 : (('s, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 093.
        Stack shape : RELATION ID.
        Start symbol: start. *)

  | MenhirState094 : ((('s, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_EQUALOP, _menhir_box_start) _menhir_state
    (** State 094.
        Stack shape : RELATION ID EQUALOP.
        Start symbol: start. *)

  | MenhirState100 : (('s, _menhir_box_start) _menhir_cell1_QUAL _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 100.
        Stack shape : QUAL ID.
        Start symbol: start. *)

  | MenhirState101 : (('s, _menhir_box_start) _menhir_cell1_REF, _menhir_box_start) _menhir_state
    (** State 101.
        Stack shape : REF.
        Start symbol: start. *)

  | MenhirState108 : ((('s, _menhir_box_start) _menhir_cell1_QUAL _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_tyd, _menhir_box_start) _menhir_state
    (** State 108.
        Stack shape : QUAL ID tyd.
        Start symbol: start. *)

  | MenhirState110 : ((('s, _menhir_box_start) _menhir_cell1_tyd, _menhir_box_start) _menhir_cell1_tyd, _menhir_box_start) _menhir_state
    (** State 110.
        Stack shape : tyd tyd.
        Start symbol: start. *)

  | MenhirState117 : (('s, _menhir_box_start) _menhir_cell1_PRIMITIVE _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 117.
        Stack shape : PRIMITIVE ID.
        Start symbol: start. *)

  | MenhirState120 : (('s, _menhir_box_start) _menhir_cell1_LAMBDA _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 120.
        Stack shape : LAMBDA ID.
        Start symbol: start. *)

  | MenhirState124 : (('s, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_state
    (** State 124.
        Stack shape : LPAREN.
        Start symbol: start. *)

  | MenhirState126 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 126.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState131 : ((('s, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_paramseq _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 131.
        Stack shape : LPAREN paramseq ID.
        Start symbol: start. *)

  | MenhirState132 : (('s, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_state
    (** State 132.
        Stack shape : LPAREN.
        Start symbol: start. *)

  | MenhirState134 : ((('s, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 134.
        Stack shape : LPAREN ID.
        Start symbol: start. *)

  | MenhirState135 : (('s, _menhir_box_start) _menhir_cell1_LESSTHAN, _menhir_box_start) _menhir_state
    (** State 135.
        Stack shape : LESSTHAN.
        Start symbol: start. *)

  | MenhirState137 : (('s, _menhir_box_start) _menhir_cell1_qualdef, _menhir_box_start) _menhir_state
    (** State 137.
        Stack shape : qualdef.
        Start symbol: start. *)

  | MenhirState141 : ((('s, _menhir_box_start) _menhir_cell1_LESSTHAN, _menhir_box_start) _menhir_cell1_parameters, _menhir_box_start) _menhir_state
    (** State 141.
        Stack shape : LESSTHAN parameters.
        Start symbol: start. *)

  | MenhirState142 : (('s, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_state
    (** State 142.
        Stack shape : LCURLY.
        Start symbol: start. *)

  | MenhirState144 : ((('s, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 144.
        Stack shape : LCURLY ID.
        Start symbol: start. *)

  | MenhirState146 : (((('s, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_tyd, _menhir_box_start) _menhir_state
    (** State 146.
        Stack shape : LCURLY ID tyd.
        Start symbol: start. *)

  | MenhirState148 : (('s, _menhir_box_start) _menhir_cell1_NOT, _menhir_box_start) _menhir_state
    (** State 148.
        Stack shape : NOT.
        Start symbol: start. *)

  | MenhirState149 : (('s, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_state
    (** State 149.
        Stack shape : LPAREN.
        Start symbol: start. *)

  | MenhirState168 : (('s, _menhir_box_start) _menhir_cell1_LAMBDA, _menhir_box_start) _menhir_state
    (** State 168.
        Stack shape : LAMBDA.
        Start symbol: start. *)

  | MenhirState171 : (('s, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 171.
        Stack shape : LPAREN ID.
        Start symbol: start. *)

  | MenhirState175 : (('s, _menhir_box_start) _menhir_cell1_vartybind, _menhir_box_start) _menhir_state
    (** State 175.
        Stack shape : vartybind.
        Start symbol: start. *)

  | MenhirState178 : ((('s, _menhir_box_start) _menhir_cell1_LAMBDA, _menhir_box_start) _menhir_cell1_tybindseq, _menhir_box_start) _menhir_state
    (** State 178.
        Stack shape : LAMBDA tybindseq.
        Start symbol: start. *)

  | MenhirState179 : (('s, _menhir_box_start) _menhir_cell1_EXISTS, _menhir_box_start) _menhir_state
    (** State 179.
        Stack shape : EXISTS.
        Start symbol: start. *)

  | MenhirState181 : ((('s, _menhir_box_start) _menhir_cell1_EXISTS, _menhir_box_start) _menhir_cell1_tybindseq, _menhir_box_start) _menhir_state
    (** State 181.
        Stack shape : EXISTS tybindseq.
        Start symbol: start. *)

  | MenhirState182 : (('s, _menhir_box_start) _menhir_cell1_ANGLEFT, _menhir_box_start) _menhir_state
    (** State 182.
        Stack shape : ANGLEFT.
        Start symbol: start. *)

  | MenhirState187 : (('s, _menhir_box_start) _menhir_cell1_rexpr, _menhir_box_start) _menhir_state
    (** State 187.
        Stack shape : rexpr.
        Start symbol: start. *)

  | MenhirState189 : (('s, _menhir_box_start) _menhir_cell1_rexpr, _menhir_box_start) _menhir_state
    (** State 189.
        Stack shape : rexpr.
        Start symbol: start. *)

  | MenhirState191 : (('s, _menhir_box_start) _menhir_cell1_rexpr, _menhir_box_start) _menhir_state
    (** State 191.
        Stack shape : rexpr.
        Start symbol: start. *)

  | MenhirState193 : (('s, _menhir_box_start) _menhir_cell1_rexpr, _menhir_box_start) _menhir_state
    (** State 193.
        Stack shape : rexpr.
        Start symbol: start. *)

  | MenhirState195 : (('s, _menhir_box_start) _menhir_cell1_rexpr, _menhir_box_start) _menhir_state
    (** State 195.
        Stack shape : rexpr.
        Start symbol: start. *)

  | MenhirState199 : (('s, _menhir_box_start) _menhir_cell1_patom, _menhir_box_start) _menhir_state
    (** State 199.
        Stack shape : patom.
        Start symbol: start. *)

  | MenhirState202 : (('s, _menhir_box_start) _menhir_cell1_patom, _menhir_box_start) _menhir_state
    (** State 202.
        Stack shape : patom.
        Start symbol: start. *)

  | MenhirState204 : (('s, _menhir_box_start) _menhir_cell1_patom, _menhir_box_start) _menhir_state
    (** State 204.
        Stack shape : patom.
        Start symbol: start. *)

  | MenhirState206 : (('s, _menhir_box_start) _menhir_cell1_patom, _menhir_box_start) _menhir_state
    (** State 206.
        Stack shape : patom.
        Start symbol: start. *)

  | MenhirState217 : ((('s, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_tyd, _menhir_box_start) _menhir_state
    (** State 217.
        Stack shape : LCURLY tyd.
        Start symbol: start. *)

  | MenhirState221 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 221.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState225 : ((('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_pred _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 225.
        Stack shape : ID pred ID.
        Start symbol: start. *)

  | MenhirState227 : (('s, _menhir_box_start) _menhir_cell1_vartyatom, _menhir_box_start) _menhir_state
    (** State 227.
        Stack shape : vartyatom.
        Start symbol: start. *)

  | MenhirState234 : (((('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_pred _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_refty, _menhir_box_start) _menhir_state
    (** State 234.
        Stack shape : ID pred ID refty.
        Start symbol: start. *)

  | MenhirState245 : (('s, _menhir_box_start) _menhir_cell1_varty, _menhir_box_start) _menhir_state
    (** State 245.
        Stack shape : varty.
        Start symbol: start. *)

  | MenhirState247 : ((('s, _menhir_box_start) _menhir_cell1_varty, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 247.
        Stack shape : varty ID.
        Start symbol: start. *)

  | MenhirState252 : (('s, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_state
    (** State 252.
        Stack shape : ID.
        Start symbol: start. *)

  | MenhirState256 : (('s, _menhir_box_start) _menhir_cell1_FORMULA _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 256.
        Stack shape : FORMULA ID.
        Start symbol: start. *)

  | MenhirState261 : (('s, _menhir_box_start) _menhir_cell1_ASSUME _menhir_cell0_ID, _menhir_box_start) _menhir_state
    (** State 261.
        Stack shape : ASSUME ID.
        Start symbol: start. *)

  | MenhirState264 : (('s, _menhir_box_start) _menhir_cell1_typespec, _menhir_box_start) _menhir_state
    (** State 264.
        Stack shape : typespec.
        Start symbol: start. *)

  | MenhirState266 : (('s, _menhir_box_start) _menhir_cell1_typedef, _menhir_box_start) _menhir_state
    (** State 266.
        Stack shape : typedef.
        Start symbol: start. *)

  | MenhirState268 : (('s, _menhir_box_start) _menhir_cell1_reldec, _menhir_box_start) _menhir_state
    (** State 268.
        Stack shape : reldec.
        Start symbol: start. *)

  | MenhirState270 : (('s, _menhir_box_start) _menhir_cell1_qualdef, _menhir_box_start) _menhir_state
    (** State 270.
        Stack shape : qualdef.
        Start symbol: start. *)

  | MenhirState272 : (('s, _menhir_box_start) _menhir_cell1_primdec, _menhir_box_start) _menhir_state
    (** State 272.
        Stack shape : primdec.
        Start symbol: start. *)

  | MenhirState274 : (('s, _menhir_box_start) _menhir_cell1_predspec, _menhir_box_start) _menhir_state
    (** State 274.
        Stack shape : predspec.
        Start symbol: start. *)


and 's _menhir_cell0_conpat = 
  | MenhirCell0_conpat of 's * (
# 83 "specParser.mly"
      ((Tycon.t * (Var.t list) option))
# 520 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_elem = 
  | MenhirCell1_elem of 's * ('s, 'r) _menhir_state * (
# 85 "specParser.mly"
      (RelLang.elem)
# 527 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_funparam = 
  | MenhirCell1_funparam of 's * ('s, 'r) _menhir_state * (
# 87 "specParser.mly"
      (Var.t)
# 534 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_instexpr = 
  | MenhirCell1_instexpr of 's * ('s, 'r) _menhir_state * (
# 92 "specParser.mly"
      (RelLang.instexpr)
# 541 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_parameters = 
  | MenhirCell1_parameters of 's * ('s, 'r) _menhir_state * (
# 125 "specParser.mly"
      (Qualifier.t list)
# 548 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_params = 
  | MenhirCell1_params of 's * ('s, 'r) _menhir_state * (
# 95 "specParser.mly"
      (RelId.t list)
# 555 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_paramseq = 
  | MenhirCell1_paramseq of 's * ('s, 'r) _menhir_state * (
# 96 "specParser.mly"
      (RelId.t list)
# 562 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_patmatch = 
  | MenhirCell1_patmatch of 's * ('s, 'r) _menhir_state * (
# 97 "specParser.mly"
      (Tycon.t * ((Var.t list) option) * RelLang.term)
# 569 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_patom = 
  | MenhirCell1_patom of 's * ('s, 'r) _menhir_state * (
# 99 "specParser.mly"
      (Predicate.t)
# 576 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_pred = 
  | MenhirCell1_pred of 's * ('s, 'r) _menhir_state * (
# 100 "specParser.mly"
      (Predicate.t)
# 583 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_predspec = 
  | MenhirCell1_predspec of 's * ('s, 'r) _menhir_state * (
# 101 "specParser.mly"
      (Formula.t)
# 590 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_primdec = 
  | MenhirCell1_primdec of 's * ('s, 'r) _menhir_state * (
# 102 "specParser.mly"
      (PrimitiveRelation.t)
# 597 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_qualdef = 
  | MenhirCell1_qualdef of 's * ('s, 'r) _menhir_state * (
# 104 "specParser.mly"
      (Qualifier.t)
# 604 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_ratom = 
  | MenhirCell1_ratom of 's * ('s, 'r) _menhir_state * (
# 105 "specParser.mly"
      (RelLang.expr)
# 611 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_refty = 
  | MenhirCell1_refty of 's * ('s, 'r) _menhir_state * (
# 106 "specParser.mly"
      (RefinementType.t)
# 618 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_reldec = 
  | MenhirCell1_reldec of 's * ('s, 'r) _menhir_state * (
# 108 "specParser.mly"
      (StructuralRelation.t)
# 625 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_rexpr = 
  | MenhirCell1_rexpr of 's * ('s, 'r) _menhir_state * (
# 109 "specParser.mly"
      (RelLang.expr)
# 632 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_tpatmatch = 
  | MenhirCell1_tpatmatch of 's * ('s, 'r) _menhir_state * (
# 114 "specParser.mly"
      (Algebraic.constructor)
# 639 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_tybindseq = 
  | MenhirCell1_tybindseq of 's * ('s, 'r) _menhir_state * (
# 116 "specParser.mly"
      ((Var.t * TyD.t) list)
# 646 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_tyd = 
  | MenhirCell1_tyd of 's * ('s, 'r) _menhir_state * (
# 117 "specParser.mly"
      (TyD.t)
# 653 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_typedef = 
  | MenhirCell1_typedef of 's * ('s, 'r) _menhir_state * (
# 118 "specParser.mly"
      (Algebraic.t)
# 660 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_typespec = 
  | MenhirCell1_typespec of 's * ('s, 'r) _menhir_state * (
# 120 "specParser.mly"
      (RelSpec.TypeSpec.t)
# 667 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_varty = 
  | MenhirCell1_varty of 's * ('s, 'r) _menhir_state * (
# 121 "specParser.mly"
      (Var.t * RefinementType.t)
# 674 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_vartyatom = 
  | MenhirCell1_vartyatom of 's * ('s, 'r) _menhir_state * (
# 122 "specParser.mly"
      (Var.t * RefinementType.t)
# 681 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_vartybind = 
  | MenhirCell1_vartybind of 's * ('s, 'r) _menhir_state * (
# 123 "specParser.mly"
      ((Var.t * TyD.t))
# 688 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_ANGLEFT = 
  | MenhirCell1_ANGLEFT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ASSUME = 
  | MenhirCell1_ASSUME of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EQUALOP = 
  | MenhirCell1_EQUALOP of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EXISTS = 
  | MenhirCell1_EXISTS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FORMULA = 
  | MenhirCell1_FORMULA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ID = 
  | MenhirCell1_ID of 's * ('s, 'r) _menhir_state * (
# 63 "specParser.mly"
        (string)
# 710 "specParser.ml"
)

and 's _menhir_cell0_ID = 
  | MenhirCell0_ID of 's * (
# 63 "specParser.mly"
        (string)
# 717 "specParser.ml"
)

and ('s, 'r) _menhir_cell1_LAMBDA = 
  | MenhirCell1_LAMBDA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LCURLY = 
  | MenhirCell1_LCURLY of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LESSTHAN = 
  | MenhirCell1_LESSTHAN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PRIMITIVE = 
  | MenhirCell1_PRIMITIVE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_QUAL = 
  | MenhirCell1_QUAL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REF = 
  | MenhirCell1_REF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RELATION = 
  | MenhirCell1_RELATION of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TYPE = 
  | MenhirCell1_TYPE of 's * ('s, 'r) _menhir_state

and _menhir_box_start = 
  | MenhirBox_start of (
# 127 "specParser.mly"
       (SpecLang.RelSpec.t)
# 757 "specParser.ml"
) [@@unboxed]

let _menhir_action_001 =
  fun ty ->
    (
# 412 "specParser.mly"
                (RefinementType.Base ((Var.get_fresh_var "v"), 
                ty,
                Predicate.truee()))
# 767 "specParser.ml"
     : (
# 80 "specParser.mly"
      (RefinementType.t)
# 771 "specParser.ml"
    ))

let _menhir_action_002 =
  fun ty ->
    (
# 415 "specParser.mly"
                              (RefinementType.Base ((Var.get_fresh_var "v"), 
                ty, 
                Predicate.truee()))
# 781 "specParser.ml"
     : (
# 80 "specParser.mly"
      (RefinementType.t)
# 785 "specParser.ml"
    ))

let _menhir_action_003 =
  fun pr ty ->
    (
# 418 "specParser.mly"
                                           (RefinementType.Base ((Var.get_fresh_var "v"), 
                ty, pr))
# 794 "specParser.ml"
     : (
# 80 "specParser.mly"
      (RefinementType.t)
# 798 "specParser.ml"
    ))

let _menhir_action_004 =
  fun pr ty v ->
    (
# 420 "specParser.mly"
                                                       (RefinementType.Base ((Var.fromString v), 
                ty, pr))
# 807 "specParser.ml"
     : (
# 80 "specParser.mly"
      (RefinementType.t)
# 811 "specParser.ml"
    ))

let _menhir_action_005 =
  fun i1 i2 ->
    (
# 447 "specParser.mly"
                                           (Predicate.BasePredicate.varEq 
                      (Var.fromString i1, Var.fromString i2))
# 820 "specParser.ml"
     : (
# 81 "specParser.mly"
      (Predicate.BasePredicate.t)
# 824 "specParser.ml"
    ))

let _menhir_action_006 =
  fun i1 ->
    (
# 449 "specParser.mly"
                                          (Predicate.BasePredicate.varBoolEq 
                      (Var.fromString i1, true))
# 833 "specParser.ml"
     : (
# 81 "specParser.mly"
      (Predicate.BasePredicate.t)
# 837 "specParser.ml"
    ))

let _menhir_action_007 =
  fun i1 ->
    (
# 451 "specParser.mly"
                                           (Predicate.BasePredicate.varBoolEq 
                      (Var.fromString i1, false))
# 846 "specParser.ml"
     : (
# 81 "specParser.mly"
      (Predicate.BasePredicate.t)
# 850 "specParser.ml"
    ))

let _menhir_action_008 =
  fun i1 i2 ->
    (
# 453 "specParser.mly"
                                                (Predicate.BasePredicate.varGt 
                      (Var.fromString i1, Var.fromString i2))
# 859 "specParser.ml"
     : (
# 81 "specParser.mly"
      (Predicate.BasePredicate.t)
# 863 "specParser.ml"
    ))

let _menhir_action_009 =
  fun i1 rhs ->
    (
# 455 "specParser.mly"
                                                  (Predicate.BasePredicate.varIntGt 
                      (Var.fromString i1, rhs))
# 872 "specParser.ml"
     : (
# 81 "specParser.mly"
      (Predicate.BasePredicate.t)
# 876 "specParser.ml"
    ))

let _menhir_action_010 =
  fun i1 rhs ->
    (
# 457 "specParser.mly"
                                              (Predicate.BasePredicate.varIntEq 
                      (Var.fromString i1, rhs))
# 885 "specParser.ml"
     : (
# 81 "specParser.mly"
      (Predicate.BasePredicate.t)
# 889 "specParser.ml"
    ))

let _menhir_action_011 =
  fun i1 rhs ->
    (
# 459 "specParser.mly"
                                                   (
       				let rhstrimmed = String.sub (rhs) (1) ((String.length rhs) - 2) in 
       				Predicate.BasePredicate.varStrEq 
                      (Var.fromString i1, rhstrimmed))
# 900 "specParser.ml"
     : (
# 81 "specParser.mly"
      (Predicate.BasePredicate.t)
# 904 "specParser.ml"
    ))

let _menhir_action_012 =
  fun i ->
    (
# 300 "specParser.mly"
               (Vector.fromList [Var.fromString i])
# 912 "specParser.ml"
     : (
# 82 "specParser.mly"
      (Var.t list)
# 916 "specParser.ml"
    ))

let _menhir_action_013 =
  fun is ->
    (
# 301 "specParser.mly"
                                 (Vector.fromList is)
# 924 "specParser.ml"
     : (
# 82 "specParser.mly"
      (Var.t list)
# 928 "specParser.ml"
    ))

let _menhir_action_014 =
  fun i ->
    (
# 297 "specParser.mly"
               ((Tycon.fromString i, None))
# 936 "specParser.ml"
     : (
# 83 "specParser.mly"
      ((Tycon.t * (Var.t list) option))
# 940 "specParser.ml"
    ))

let _menhir_action_015 =
  fun co i ->
    (
# 298 "specParser.mly"
                          ((Tycon.fromString i, Some co))
# 948 "specParser.ml"
     : (
# 83 "specParser.mly"
      ((Tycon.t * (Var.t list) option))
# 952 "specParser.ml"
    ))

let _menhir_action_016 =
  fun d td ->
    (
# 139 "specParser.mly"
                  (
                    match d with 
                          RelSpec.T ({typedefs;preds;quals;reldecs; primdecs; typespecs}) ->    
                              RelSpec.T {
                              typedefs = td :: typedefs;
                              preds = preds;
                              quals = quals;
                              reldecs = reldecs; 
                              primdecs = primdecs;
                              typespecs = typespecs}
                  )
# 970 "specParser.ml"
     : (
# 84 "specParser.mly"
      (RelSpec.t)
# 974 "specParser.ml"
    ))

let _menhir_action_017 =
  fun d r ->
    (
# 153 "specParser.mly"
                  (
                    match d with 
                    RelSpec.T ({typedefs;preds;quals;reldecs; primdecs; typespecs}) -> 
                    RelSpec.T {typedefs=typedefs;
                              preds = preds;
		    		                  quals = quals;
                              reldecs = r ::reldecs; 
                              primdecs = primdecs;
                            typespecs = typespecs}
                          )
# 991 "specParser.ml"
     : (
# 84 "specParser.mly"
      (RelSpec.t)
# 995 "specParser.ml"
    ))

let _menhir_action_018 =
  fun d p ->
    (
# 164 "specParser.mly"
                (match d with 
                  RelSpec.T ({typedefs;preds;quals;reldecs; primdecs; typespecs}) -> 
                    RelSpec.T {
                              typedefs=typedefs;
                              preds = preds;
		    		                   quals = quals;
                              primdecs = p :: primdecs; 
                              reldecs=reldecs; 
                              typespecs = typespecs}
                )
# 1012 "specParser.ml"
     : (
# 84 "specParser.mly"
      (RelSpec.t)
# 1016 "specParser.ml"
    ))

let _menhir_action_019 =
  fun d t ->
    (
# 176 "specParser.mly"
                (
                  match d with
                   RelSpec.T {typedefs;preds;quals;reldecs; primdecs;typespecs} -> 
                    RelSpec.T {typedefs=typedefs;preds= preds;quals;reldecs = reldecs; primdecs=primdecs;
                      typespecs = t :: typespecs}
                )
# 1029 "specParser.ml"
     : (
# 84 "specParser.mly"
      (RelSpec.t)
# 1033 "specParser.ml"
    ))

let _menhir_action_020 =
  fun d p ->
    (
# 183 "specParser.mly"
    (
		 match d with 
		  RelSpec.T {typedefs;preds;quals;reldecs; primdecs; typespecs} -> 
                    RelSpec.T {typedefs=typedefs;
                              preds= p :: preds;
                              quals = quals;
                              reldecs = reldecs; 
                              primdecs=primdecs;
                              typespecs = typespecs}

		)
# 1051 "specParser.ml"
     : (
# 84 "specParser.mly"
      (RelSpec.t)
# 1055 "specParser.ml"
    ))

let _menhir_action_021 =
  fun d q ->
    (
# 195 "specParser.mly"
    (
		 match d with 
		    RelSpec.T {typedefs;preds;quals;reldecs; primdecs; typespecs} -> 
                    RelSpec.T {typedefs=typedefs;
                              preds= preds;
                              quals = q :: quals;
                              reldecs = reldecs; primdecs=primdecs;
                      typespecs = typespecs}

		)
# 1072 "specParser.ml"
     : (
# 84 "specParser.mly"
      (RelSpec.t)
# 1076 "specParser.ml"
    ))

let _menhir_action_022 =
  fun () ->
    (
# 207 "specParser.mly"
          (RelSpec.T {
                    typedefs=[];
  			            preds = Vector.new0 ();
  			            quals = [];
                    reldecs = [];
                    primdecs = Vector.new0 ();
                    typespecs = []})
# 1090 "specParser.ml"
     : (
# 84 "specParser.mly"
      (RelSpec.t)
# 1094 "specParser.ml"
    ))

let _menhir_action_023 =
  fun ii ->
    (
# 345 "specParser.mly"
              (Int(ii))
# 1102 "specParser.ml"
     : (
# 85 "specParser.mly"
      (RelLang.elem)
# 1106 "specParser.ml"
    ))

let _menhir_action_024 =
  fun t ->
    (
# 346 "specParser.mly"
              (Bool(true))
# 1114 "specParser.ml"
     : (
# 85 "specParser.mly"
      (RelLang.elem)
# 1118 "specParser.ml"
    ))

let _menhir_action_025 =
  fun f ->
    (
# 347 "specParser.mly"
               (Bool(false))
# 1126 "specParser.ml"
     : (
# 85 "specParser.mly"
      (RelLang.elem)
# 1130 "specParser.ml"
    ))

let _menhir_action_026 =
  fun i ->
    (
# 348 "specParser.mly"
            (Var(Var.fromString i))
# 1138 "specParser.ml"
     : (
# 85 "specParser.mly"
      (RelLang.elem)
# 1142 "specParser.ml"
    ))

let _menhir_action_027 =
  fun el ->
    (
# 342 "specParser.mly"
                  ([el])
# 1150 "specParser.ml"
     : (
# 86 "specParser.mly"
      (RelLang.elem list)
# 1154 "specParser.ml"
    ))

let _menhir_action_028 =
  fun el els ->
    (
# 343 "specParser.mly"
                                    (el::els)
# 1162 "specParser.ml"
     : (
# 86 "specParser.mly"
      (RelLang.elem list)
# 1166 "specParser.ml"
    ))

let _menhir_action_029 =
  fun i ->
    (
# 340 "specParser.mly"
                (Var.fromString i)
# 1174 "specParser.ml"
     : (
# 87 "specParser.mly"
      (Var.t)
# 1178 "specParser.ml"
    ))

let _menhir_action_030 =
  fun p ->
    (
# 337 "specParser.mly"
                       ([p])
# 1186 "specParser.ml"
     : (
# 89 "specParser.mly"
      (Var.t list)
# 1190 "specParser.ml"
    ))

let _menhir_action_031 =
  fun p ps ->
    (
# 338 "specParser.mly"
                                 (p::ps)
# 1198 "specParser.ml"
     : (
# 89 "specParser.mly"
      (Var.t list)
# 1202 "specParser.ml"
    ))

let _menhir_action_032 =
  fun i ->
    (
# 303 "specParser.mly"
             ([Var.fromString i])
# 1210 "specParser.ml"
     : (
# 91 "specParser.mly"
      (Var.t list)
# 1214 "specParser.ml"
    ))

let _menhir_action_033 =
  fun i is ->
    (
# 304 "specParser.mly"
                            ((Var.fromString i)::is)
# 1222 "specParser.ml"
     : (
# 91 "specParser.mly"
      (Var.t list)
# 1226 "specParser.ml"
    ))

let _menhir_action_034 =
  fun i ->
    (
# 306 "specParser.mly"
                (RInst { sargs = empty (); 
                targs = empty(); args = empty (); 
                rel = RelId.fromString i})
# 1236 "specParser.ml"
     : (
# 92 "specParser.mly"
      (RelLang.instexpr)
# 1240 "specParser.ml"
    ))

let _menhir_action_035 =
  fun i ies ->
    (
# 309 "specParser.mly"
                              (RInst {
                sargs = empty (); targs = empty();
                args = Vector.fromList ies;
                rel = RelId.fromString i})
# 1251 "specParser.ml"
     : (
# 92 "specParser.mly"
      (RelLang.instexpr)
# 1255 "specParser.ml"
    ))

let _menhir_action_036 =
  fun ie ->
    (
# 314 "specParser.mly"
                                      ([ie])
# 1263 "specParser.ml"
     : (
# 93 "specParser.mly"
      (RelLang.instexpr list)
# 1267 "specParser.ml"
    ))

let _menhir_action_037 =
  fun ie ies ->
    (
# 315 "specParser.mly"
                                                    (ie :: ies)
# 1275 "specParser.ml"
     : (
# 93 "specParser.mly"
      (RelLang.instexpr list)
# 1279 "specParser.ml"
    ))

let _menhir_action_038 =
  fun ef post pre resty resvar ->
    (
# 423 "specParser.mly"
                                                                                         (RefTy.MArrow (Effect.fromString ef, pre, (resvar, resty), post))
# 1287 "specParser.ml"
     : (
# 94 "specParser.mly"
      (RefinementType.t)
# 1291 "specParser.ml"
    ))

let _menhir_action_039 =
  fun p ->
    (
# 377 "specParser.mly"
                         ([p])
# 1299 "specParser.ml"
     : (
# 125 "specParser.mly"
      (Qualifier.t list)
# 1303 "specParser.ml"
    ))

let _menhir_action_040 =
  fun p ps ->
    (
# 378 "specParser.mly"
                                                (p::ps)
# 1311 "specParser.ml"
     : (
# 125 "specParser.mly"
      (Qualifier.t list)
# 1315 "specParser.ml"
    ))

let _menhir_action_041 =
  fun i ->
    (
# 270 "specParser.mly"
                ([RelId.fromString i])
# 1323 "specParser.ml"
     : (
# 95 "specParser.mly"
      (RelId.t list)
# 1327 "specParser.ml"
    ))

let _menhir_action_042 =
  fun i p ->
    (
# 271 "specParser.mly"
                       ((RelId.fromString i)::p)
# 1335 "specParser.ml"
     : (
# 95 "specParser.mly"
      (RelId.t list)
# 1339 "specParser.ml"
    ))

let _menhir_action_043 =
  fun i ->
    (
# 273 "specParser.mly"
                    ([RelId.fromString i])
# 1347 "specParser.ml"
     : (
# 96 "specParser.mly"
      (RelId.t list)
# 1351 "specParser.ml"
    ))

let _menhir_action_044 =
  fun i pseq ->
    (
# 274 "specParser.mly"
                                  ((RelId.fromString i)::pseq)
# 1359 "specParser.ml"
     : (
# 96 "specParser.mly"
      (RelId.t list)
# 1363 "specParser.ml"
    ))

let _menhir_action_045 =
  fun cp re ->
    (
# 293 "specParser.mly"
              (match cp with (c,vlop) -> (c, vlop, Expr re))
# 1371 "specParser.ml"
     : (
# 97 "specParser.mly"
      (Tycon.t * ((Var.t list) option) * RelLang.term)
# 1375 "specParser.ml"
    ))

let _menhir_action_046 =
  fun i re ->
    (
# 294 "specParser.mly"
                                 ((Tycon.fromString i, None, Expr re))
# 1383 "specParser.ml"
     : (
# 97 "specParser.mly"
      (Tycon.t * ((Var.t list) option) * RelLang.term)
# 1387 "specParser.ml"
    ))

let _menhir_action_047 =
  fun pm pms ->
    (
# 276 "specParser.mly"
                                               (pm :: pms)
# 1395 "specParser.ml"
     : (
# 98 "specParser.mly"
      ((Tycon.t * ((Var.t list) option) * RelLang.term) list)
# 1399 "specParser.ml"
    ))

let _menhir_action_048 =
  fun pm ->
    (
# 277 "specParser.mly"
                          ([pm])
# 1407 "specParser.ml"
     : (
# 98 "specParser.mly"
      ((Tycon.t * ((Var.t list) option) * RelLang.term) list)
# 1411 "specParser.ml"
    ))

let _menhir_action_049 =
  fun () ->
    (
# 440 "specParser.mly"
             (Predicate.truee())
# 1419 "specParser.ml"
     : (
# 99 "specParser.mly"
      (Predicate.t)
# 1423 "specParser.ml"
    ))

let _menhir_action_050 =
  fun pa ->
    (
# 441 "specParser.mly"
                     (Predicate.Not pa)
# 1431 "specParser.ml"
     : (
# 99 "specParser.mly"
      (Predicate.t)
# 1435 "specParser.ml"
    ))

let _menhir_action_051 =
  fun pr ->
    (
# 442 "specParser.mly"
                              (pr)
# 1443 "specParser.ml"
     : (
# 99 "specParser.mly"
      (Predicate.t)
# 1447 "specParser.ml"
    ))

let _menhir_action_052 =
  fun ba ->
    (
# 443 "specParser.mly"
                  (Predicate.Base ba)
# 1455 "specParser.ml"
     : (
# 99 "specParser.mly"
      (Predicate.t)
# 1459 "specParser.ml"
    ))

let _menhir_action_053 =
  fun ra ->
    (
# 444 "specParser.mly"
                  (Predicate.Rel ra)
# 1467 "specParser.ml"
     : (
# 99 "specParser.mly"
      (Predicate.t)
# 1471 "specParser.ml"
    ))

let _menhir_action_054 =
  fun pa ->
    (
# 432 "specParser.mly"
                 (pa)
# 1479 "specParser.ml"
     : (
# 100 "specParser.mly"
      (Predicate.t)
# 1483 "specParser.ml"
    ))

let _menhir_action_055 =
  fun pa pr ->
    (
# 433 "specParser.mly"
                              (Predicate.If (pa,pr))
# 1491 "specParser.ml"
     : (
# 100 "specParser.mly"
      (Predicate.t)
# 1495 "specParser.ml"
    ))

let _menhir_action_056 =
  fun pa pr ->
    (
# 434 "specParser.mly"
                             (Predicate.Iff (pa,pr))
# 1503 "specParser.ml"
     : (
# 100 "specParser.mly"
      (Predicate.t)
# 1507 "specParser.ml"
    ))

let _menhir_action_057 =
  fun pa pr ->
    (
# 435 "specParser.mly"
                              (Predicate.Conj (pa,pr))
# 1515 "specParser.ml"
     : (
# 100 "specParser.mly"
      (Predicate.t)
# 1519 "specParser.ml"
    ))

let _menhir_action_058 =
  fun pa pr ->
    (
# 436 "specParser.mly"
                              (Predicate.Disj (pa,pr))
# 1527 "specParser.ml"
     : (
# 100 "specParser.mly"
      (Predicate.t)
# 1531 "specParser.ml"
    ))

let _menhir_action_059 =
  fun binds pr ->
    (
# 437 "specParser.mly"
                                           (Predicate.Forall (binds, pr) )
# 1539 "specParser.ml"
     : (
# 100 "specParser.mly"
      (Predicate.t)
# 1543 "specParser.ml"
    ))

let _menhir_action_060 =
  fun binds pr ->
    (
# 438 "specParser.mly"
                                           (Predicate.Exists (binds, pr))
# 1551 "specParser.ml"
     : (
# 100 "specParser.mly"
      (Predicate.t)
# 1555 "specParser.ml"
    ))

let _menhir_action_061 =
  fun i p ->
    (
# 233 "specParser.mly"
                                       (Formula.Form{name=Var.fromString i;pred = p})
# 1563 "specParser.ml"
     : (
# 101 "specParser.mly"
      (Formula.t)
# 1567 "specParser.ml"
    ))

let _menhir_action_062 =
  fun i p ->
    (
# 235 "specParser.mly"
                                                    (PrimitiveRelation.T
                    {id=RelId.fromString i; 
                    def=PrimitiveRelation.alphaRename p})
# 1577 "specParser.ml"
     : (
# 102 "specParser.mly"
      (PrimitiveRelation.t)
# 1581 "specParser.ml"
    ))

let _menhir_action_063 =
  fun re ->
    (
# 239 "specParser.mly"
                   (PrimitiveRelation.Nullary re)
# 1589 "specParser.ml"
     : (
# 103 "specParser.mly"
      (PrimitiveRelation.def)
# 1593 "specParser.ml"
    ))

let _menhir_action_064 =
  fun i p ->
    (
# 240 "specParser.mly"
                                    (PrimitiveRelation.Nary
                (Var.fromString i, p))
# 1602 "specParser.ml"
     : (
# 103 "specParser.mly"
      (PrimitiveRelation.def)
# 1606 "specParser.ml"
    ))

let _menhir_action_065 =
  fun i p ->
    (
# 231 "specParser.mly"
                                  (Qualifier.Qual {name=Var.fromString i; shape = p})
# 1614 "specParser.ml"
     : (
# 104 "specParser.mly"
      (Qualifier.t)
# 1618 "specParser.ml"
    ))

let _menhir_action_066 =
  fun () ->
    (
# 326 "specParser.mly"
                                    (T(Vector.fromList []))
# 1626 "specParser.ml"
     : (
# 105 "specParser.mly"
      (RelLang.expr)
# 1630 "specParser.ml"
    ))

let _menhir_action_067 =
  fun els ->
    (
# 327 "specParser.mly"
                                                (T(Vector.fromList els))
# 1638 "specParser.ml"
     : (
# 105 "specParser.mly"
      (RelLang.expr)
# 1642 "specParser.ml"
    ))

let _menhir_action_068 =
  fun ie ps ->
    (
# 328 "specParser.mly"
                                               (MultiR (ie, ps))
# 1650 "specParser.ml"
     : (
# 105 "specParser.mly"
      (RelLang.expr)
# 1654 "specParser.ml"
    ))

let _menhir_action_069 =
  fun i ie ->
    (
# 329 "specParser.mly"
                                       (R (ie, Var.fromString i))
# 1662 "specParser.ml"
     : (
# 105 "specParser.mly"
      (RelLang.expr)
# 1666 "specParser.ml"
    ))

let _menhir_action_070 =
  fun re ->
    (
# 330 "specParser.mly"
                               (re)
# 1674 "specParser.ml"
     : (
# 105 "specParser.mly"
      (RelLang.expr)
# 1678 "specParser.ml"
    ))

let _menhir_action_071 =
  fun el ->
    (
# 331 "specParser.mly"
                              (T[el])
# 1686 "specParser.ml"
     : (
# 105 "specParser.mly"
      (RelLang.expr)
# 1690 "specParser.ml"
    ))

let _menhir_action_072 =
  fun el ->
    (
# 332 "specParser.mly"
                (V (el))
# 1698 "specParser.ml"
     : (
# 105 "specParser.mly"
      (RelLang.expr)
# 1702 "specParser.ml"
    ))

let _menhir_action_073 =
  fun rta ->
    (
# 367 "specParser.mly"
                      ( rta)
# 1710 "specParser.ml"
     : (
# 106 "specParser.mly"
      (RefinementType.t)
# 1714 "specParser.ml"
    ))

let _menhir_action_074 =
  fun rt vrta ->
    (
# 368 "specParser.mly"
                                      ( RefTy.Arrow (((fst (vrta)) , (snd vrta)), rt))
# 1722 "specParser.ml"
     : (
# 106 "specParser.mly"
      (RefinementType.t)
# 1726 "specParser.ml"
    ))

let _menhir_action_075 =
  fun mtype ->
    (
# 369 "specParser.mly"
                  (mtype)
# 1734 "specParser.ml"
     : (
# 106 "specParser.mly"
      (RefinementType.t)
# 1738 "specParser.ml"
    ))

let _menhir_action_076 =
  fun body prt ps ->
    (
# 370 "specParser.mly"
                                                                       (
                                                  let tyvars = RefTy.getTyVars body in 
                                                  RefTy.PRT {tyvars = tyvars;
                                                  params = ps;
                                                  refty= body}
                                                  )
# 1751 "specParser.ml"
     : (
# 106 "specParser.mly"
      (RefinementType.t)
# 1755 "specParser.ml"
    ))

let _menhir_action_077 =
  fun bt ->
    (
# 381 "specParser.mly"
                      ( bt)
# 1763 "specParser.ml"
     : (
# 107 "specParser.mly"
      (RefinementType.t)
# 1767 "specParser.ml"
    ))

let _menhir_action_078 =
  fun vas ->
    (
# 382 "specParser.mly"
                                        (RefTy.Sigma vas)
# 1775 "specParser.ml"
     : (
# 107 "specParser.mly"
      (RefinementType.t)
# 1779 "specParser.ml"
    ))

let _menhir_action_079 =
  fun i patmseq ->
    (
# 245 "specParser.mly"
          (StructuralRelation.T {id=RelId.fromString i;
                params = empty ();
                mapp = patmseq})
# 1789 "specParser.ml"
     : (
# 108 "specParser.mly"
      (StructuralRelation.t)
# 1793 "specParser.ml"
    ))

let _menhir_action_080 =
  fun i p patmseq ->
    (
# 249 "specParser.mly"
          (StructuralRelation.T {id=RelId.fromString i;
                params = p;
                mapp = patmseq})
# 1803 "specParser.ml"
     : (
# 108 "specParser.mly"
      (StructuralRelation.t)
# 1807 "specParser.ml"
    ))

let _menhir_action_081 =
  fun i ie ->
    (
# 253 "specParser.mly"
          (StructuralRelation.T{id=RelId.fromString i;
                params = empty ();
                mapp = [(defaultCons,None,
                  Star ie)]})
# 1818 "specParser.ml"
     : (
# 108 "specParser.mly"
      (StructuralRelation.t)
# 1822 "specParser.ml"
    ))

let _menhir_action_082 =
  fun i ie p ->
    (
# 258 "specParser.mly"
          (StructuralRelation.T{id=RelId.fromString i;
                params = p;
                mapp = [(defaultCons,None,
                  Star ie)]})
# 1833 "specParser.ml"
     : (
# 108 "specParser.mly"
      (StructuralRelation.t)
# 1837 "specParser.ml"
    ))

let _menhir_action_083 =
  fun ra re ->
    (
# 318 "specParser.mly"
                                   (X(ra,re))
# 1845 "specParser.ml"
     : (
# 109 "specParser.mly"
      (RelLang.expr)
# 1849 "specParser.ml"
    ))

let _menhir_action_084 =
  fun ra re ->
    (
# 319 "specParser.mly"
                                (U(ra,re))
# 1857 "specParser.ml"
     : (
# 109 "specParser.mly"
      (RelLang.expr)
# 1861 "specParser.ml"
    ))

let _menhir_action_085 =
  fun ra re ->
    (
# 320 "specParser.mly"
                                (D(ra,re))
# 1869 "specParser.ml"
     : (
# 109 "specParser.mly"
      (RelLang.expr)
# 1873 "specParser.ml"
    ))

let _menhir_action_086 =
  fun ra re ->
    (
# 321 "specParser.mly"
                               (ADD(ra,re))
# 1881 "specParser.ml"
     : (
# 109 "specParser.mly"
      (RelLang.expr)
# 1885 "specParser.ml"
    ))

let _menhir_action_087 =
  fun ra re ->
    (
# 322 "specParser.mly"
                                  (SUBS(ra,re))
# 1893 "specParser.ml"
     : (
# 109 "specParser.mly"
      (RelLang.expr)
# 1897 "specParser.ml"
    ))

let _menhir_action_088 =
  fun ra ->
    (
# 323 "specParser.mly"
                 (ra)
# 1905 "specParser.ml"
     : (
# 109 "specParser.mly"
      (RelLang.expr)
# 1909 "specParser.ml"
    ))

let _menhir_action_089 =
  fun re ->
    (
# 465 "specParser.mly"
                                   (Predicate.RelPredicate.Q (re))
# 1917 "specParser.ml"
     : (
# 110 "specParser.mly"
      (Predicate.RelPredicate.t)
# 1921 "specParser.ml"
    ))

let _menhir_action_090 =
  fun re1 re2 ->
    (
# 466 "specParser.mly"
                                     (Predicate.RelPredicate.Eq(re1,re2))
# 1929 "specParser.ml"
     : (
# 110 "specParser.mly"
      (Predicate.RelPredicate.t)
# 1933 "specParser.ml"
    ))

let _menhir_action_091 =
  fun re1 re2 ->
    (
# 467 "specParser.mly"
                                    (Predicate.RelPredicate.Sub(re1,re2))
# 1941 "specParser.ml"
     : (
# 110 "specParser.mly"
      (Predicate.RelPredicate.t)
# 1945 "specParser.ml"
    ))

let _menhir_action_092 =
  fun re1 re2 ->
    (
# 468 "specParser.mly"
                                      (Predicate.RelPredicate.SubEq(re1,re2))
# 1953 "specParser.ml"
     : (
# 110 "specParser.mly"
      (Predicate.RelPredicate.t)
# 1957 "specParser.ml"
    ))

let _menhir_action_093 =
  fun re1 re2 ->
    (
# 469 "specParser.mly"
                                   (Predicate.RelPredicate.NEq(re1, re2) )
# 1965 "specParser.ml"
     : (
# 110 "specParser.mly"
      (Predicate.RelPredicate.t)
# 1969 "specParser.ml"
    ))

let _menhir_action_094 =
  fun re1 re2 ->
    (
# 470 "specParser.mly"
                                         (Predicate.RelPredicate.Grt(re1, re2))
# 1977 "specParser.ml"
     : (
# 110 "specParser.mly"
      (Predicate.RelPredicate.t)
# 1981 "specParser.ml"
    ))

let _menhir_action_095 =
  fun s ss ->
    (
# 264 "specParser.mly"
                                       (s :: ss)
# 1989 "specParser.ml"
     : (
# 111 "specParser.mly"
      (TyD.t list)
# 1993 "specParser.ml"
    ))

let _menhir_action_096 =
  fun s ->
    (
# 266 "specParser.mly"
                  ([s])
# 2001 "specParser.ml"
     : (
# 112 "specParser.mly"
      (TyD.t list)
# 2005 "specParser.ml"
    ))

let _menhir_action_097 =
  fun s ss ->
    (
# 267 "specParser.mly"
                                         (s :: ss)
# 2013 "specParser.ml"
     : (
# 112 "specParser.mly"
      (TyD.t list)
# 2017 "specParser.ml"
    ))

let _menhir_action_098 =
  fun d ->
    (
# 134 "specParser.mly"
                   (
                  d)
# 2026 "specParser.ml"
     : (
# 113 "specParser.mly"
      (RelSpec.t)
# 2030 "specParser.ml"
    ))

let _menhir_action_099 =
  fun s ->
    (
# 130 "specParser.mly"
               (s)
# 2038 "specParser.ml"
     : (
# 127 "specParser.mly"
       (SpecLang.RelSpec.t)
# 2042 "specParser.ml"
    ))

let _menhir_action_100 =
  fun () ->
    (
# 131 "specParser.mly"
              (RelSpec.mk_empty_relspec ())
# 2050 "specParser.ml"
     : (
# 127 "specParser.mly"
       (SpecLang.RelSpec.t)
# 2054 "specParser.ml"
    ))

let _menhir_action_101 =
  fun i ->
    (
# 286 "specParser.mly"
              (Algebraic.Const {name=i;args=[]})
# 2062 "specParser.ml"
     : (
# 114 "specParser.mly"
      (Algebraic.constructor)
# 2066 "specParser.ml"
    ))

let _menhir_action_102 =
  fun i typeargs ->
    (
# 287 "specParser.mly"
                                        (Algebraic.Const {name=i;args=typeargs})
# 2074 "specParser.ml"
     : (
# 114 "specParser.mly"
      (Algebraic.constructor)
# 2078 "specParser.ml"
    ))

let _menhir_action_103 =
  fun tpm tpms ->
    (
# 280 "specParser.mly"
                                                    (tpm :: tpms)
# 2086 "specParser.ml"
     : (
# 115 "specParser.mly"
      (Algebraic.constructor list)
# 2090 "specParser.ml"
    ))

let _menhir_action_104 =
  fun tpm ->
    (
# 281 "specParser.mly"
                            ([tpm])
# 2098 "specParser.ml"
     : (
# 115 "specParser.mly"
      (Algebraic.constructor list)
# 2102 "specParser.ml"
    ))

let _menhir_action_105 =
  fun vty ->
    (
# 473 "specParser.mly"
                          ([vty])
# 2110 "specParser.ml"
     : (
# 116 "specParser.mly"
      ((Var.t * TyD.t) list)
# 2114 "specParser.ml"
    ))

let _menhir_action_106 =
  fun vt vts ->
    (
# 474 "specParser.mly"
                                            (vt :: vts)
# 2122 "specParser.ml"
     : (
# 116 "specParser.mly"
      ((Var.t * TyD.t) list)
# 2126 "specParser.ml"
    ))

let _menhir_action_107 =
  fun t ->
    (
# 407 "specParser.mly"
           (TyD.fromString t)
# 2134 "specParser.ml"
     : (
# 117 "specParser.mly"
      (TyD.t)
# 2138 "specParser.ml"
    ))

let _menhir_action_108 =
  fun t ->
    (
# 408 "specParser.mly"
                         (TyD.makeTList (TyD.fromString t))
# 2146 "specParser.ml"
     : (
# 117 "specParser.mly"
      (TyD.t)
# 2150 "specParser.ml"
    ))

let _menhir_action_109 =
  fun t ->
    (
# 409 "specParser.mly"
               (TyD.makeTRef t )
# 2158 "specParser.ml"
     : (
# 117 "specParser.mly"
      (TyD.t)
# 2162 "specParser.ml"
    ))

let _menhir_action_110 =
  fun cons tn ->
    (
# 219 "specParser.mly"
                (Algebraic.TD {
                    typename = Var.fromString tn;   
                    constructors = cons    
                    }
                )
# 2174 "specParser.ml"
     : (
# 118 "specParser.mly"
      (Algebraic.t)
# 2178 "specParser.ml"
    ))

let _menhir_action_111 =
  fun tn ->
    (
# 224 "specParser.mly"
                      (
            Algebraic.TD {
                    typename = Var.fromString tn;   
                    constructors = []    
                    }
        )
# 2191 "specParser.ml"
     : (
# 118 "specParser.mly"
      (Algebraic.t)
# 2195 "specParser.ml"
    ))

let _menhir_action_112 =
  fun tnseq typename ->
    (
# 289 "specParser.mly"
                                                   (TyD.fromString (typename):: tnseq)
# 2203 "specParser.ml"
     : (
# 119 "specParser.mly"
      (TyD.t list)
# 2207 "specParser.ml"
    ))

let _menhir_action_113 =
  fun typename ->
    (
# 290 "specParser.mly"
                            ([TyD.fromString (typename)])
# 2215 "specParser.ml"
     : (
# 119 "specParser.mly"
      (TyD.t list)
# 2219 "specParser.ml"
    ))

let _menhir_action_114 =
  fun i rt ->
    (
# 352 "specParser.mly"
                                      (
                                          TypeSpec.T {isAssume = true;
                                              name = (Var.fromString i);
                                              params = empty ();
                                              refty = rt})
# 2231 "specParser.ml"
     : (
# 120 "specParser.mly"
      (RelSpec.TypeSpec.t)
# 2235 "specParser.ml"
    ))

let _menhir_action_115 =
  fun i rt ->
    (
# 357 "specParser.mly"
                               (      TypeSpec.T {isAssume = false;
                                       name = (Var.fromString i);
                                       params = empty ();
                                       refty = rt})
# 2246 "specParser.ml"
     : (
# 120 "specParser.mly"
      (RelSpec.TypeSpec.t)
# 2250 "specParser.ml"
    ))

let _menhir_action_116 =
  fun i ps rt ->
    (
# 361 "specParser.mly"
                                                         (
                                  TypeSpec.T {isAssume = false;
                                name = Var.fromString i;
                                params = Vector.fromList ps; 
                                refty = rt})
# 2262 "specParser.ml"
     : (
# 120 "specParser.mly"
      (RelSpec.TypeSpec.t)
# 2266 "specParser.ml"
    ))

let _menhir_action_117 =
  fun i rt ->
    (
# 405 "specParser.mly"
  (((Var.fromString i), rt))
# 2274 "specParser.ml"
     : (
# 121 "specParser.mly"
      (Var.t * RefinementType.t)
# 2278 "specParser.ml"
    ))

let _menhir_action_118 =
  fun bt i ->
    (
# 385 "specParser.mly"
  (
                  match bt with 
                     RefTy.Base (v,_,_) -> ((Var.fromString i),bt)
                       | _ -> raise (Failure "Impossible case of basety")
		)
# 2290 "specParser.ml"
     : (
# 122 "specParser.mly"
      (Var.t * RefinementType.t)
# 2294 "specParser.ml"
    ))

let _menhir_action_119 =
  fun vas ->
    (
# 390 "specParser.mly"
                                 (
                match vas with
                        [x] -> x 
                        | _ -> (genVar (), RefTy.Sigma vas)
    )
# 2306 "specParser.ml"
     : (
# 122 "specParser.mly"
      (Var.t * RefinementType.t)
# 2310 "specParser.ml"
    ))

let _menhir_action_120 =
  fun fty i ->
    (
# 396 "specParser.mly"
  (         match fty with 
                     RefTy.Arrow ((_,_),_) -> 
                          ((Var.fromString i),fty)
                       | _ -> raise (Failure "Impossible case of ArrowType")
		)
# 2322 "specParser.ml"
     : (
# 122 "specParser.mly"
      (Var.t * RefinementType.t)
# 2326 "specParser.ml"
    ))

let _menhir_action_121 =
  fun ty v ->
    (
# 477 "specParser.mly"
   ((v, ty))
# 2334 "specParser.ml"
     : (
# 123 "specParser.mly"
      ((Var.t * TyD.t))
# 2338 "specParser.ml"
    ))

let _menhir_action_122 =
  fun vt ->
    (
# 401 "specParser.mly"
                    ([vt])
# 2346 "specParser.ml"
     : (
# 124 "specParser.mly"
      ((Var.t * RefinementType.t) list)
# 2350 "specParser.ml"
    ))

let _menhir_action_123 =
  fun vt vts ->
    (
# 402 "specParser.mly"
                                       (vt :: vts)
# 2358 "specParser.ml"
     : (
# 124 "specParser.mly"
      ((Var.t * RefinementType.t) list)
# 2362 "specParser.ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ALL ->
        "ALL"
    | ANGLEFT ->
        "ANGLEFT"
    | ANGRIGHT ->
        "ANGRIGHT"
    | ARMINUS ->
        "ARMINUS"
    | ARROW ->
        "ARROW"
    | ASSUME ->
        "ASSUME"
    | COLON ->
        "COLON"
    | COLONARROW ->
        "COLONARROW"
    | COMMA ->
        "COMMA"
    | CONJ ->
        "CONJ"
    | CROSSPRD ->
        "CROSSPRD"
    | DISJ ->
        "DISJ"
    | DOT ->
        "DOT"
    | EOF ->
        "EOF"
    | EOL ->
        "EOL"
    | EQUALOP ->
        "EQUALOP"
    | EXC ->
        "EXC"
    | EXISTS ->
        "EXISTS"
    | FALSE ->
        "FALSE"
    | FORMULA ->
        "FORMULA"
    | GREATERTHAN ->
        "GREATERTHAN"
    | ID _ ->
        "ID"
    | IFF ->
        "IFF"
    | IMPL ->
        "IMPL"
    | INT _ ->
        "INT"
    | LAMBDA ->
        "LAMBDA"
    | LBRACE ->
        "LBRACE"
    | LCURLY ->
        "LCURLY"
    | LESSTHAN ->
        "LESSTHAN"
    | LPAREN ->
        "LPAREN"
    | MINUS ->
        "MINUS"
    | NOT ->
        "NOT"
    | NUMEQ ->
        "NUMEQ"
    | OF ->
        "OF"
    | PIPE ->
        "PIPE"
    | PLUS ->
        "PLUS"
    | PRIMITIVE ->
        "PRIMITIVE"
    | PURE ->
        "PURE"
    | QUAL ->
        "QUAL"
    | RBRACE ->
        "RBRACE"
    | RCURLY ->
        "RCURLY"
    | REF ->
        "REF"
    | RELATION ->
        "RELATION"
    | RPAREN ->
        "RPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | STAR ->
        "STAR"
    | STATE ->
        "STATE"
    | STEXC ->
        "STEXC"
    | STRCONST _ ->
        "STRCONST"
    | SUBSET ->
        "SUBSET"
    | SUBSETEQ ->
        "SUBSETEQ"
    | TRUE ->
        "TRUE"
    | TYPE ->
        "TYPE"
    | UINST ->
        "UINST"
    | UNION ->
        "UNION"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_284_spec_000 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_start =
    fun _menhir_stack _v ->
      let _v =
        let d = _v in
        _menhir_action_098 d
      in
      let s = _v in
      let _v = _menhir_action_099 s in
      MenhirBox_start _v
  
  let rec _menhir_run_275 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_predspec -> _ -> _menhir_box_start =
    fun _menhir_stack _v ->
      let MenhirCell1_predspec (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let d = _v in
      let _v = _menhir_action_020 d p in
      _menhir_goto_decsandtys _menhir_stack _v _menhir_s
  
  and _menhir_goto_decsandtys : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_284_spec_000 _menhir_stack _v
      | MenhirState264 ->
          _menhir_run_280 _menhir_stack _v
      | MenhirState266 ->
          _menhir_run_279 _menhir_stack _v
      | MenhirState268 ->
          _menhir_run_278 _menhir_stack _v
      | MenhirState270 ->
          _menhir_run_277 _menhir_stack _v
      | MenhirState272 ->
          _menhir_run_276 _menhir_stack _v
      | MenhirState274 ->
          _menhir_run_275 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_280 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_typespec -> _ -> _menhir_box_start =
    fun _menhir_stack _v ->
      let MenhirCell1_typespec (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let d = _v in
      let _v = _menhir_action_019 d t in
      _menhir_goto_decsandtys _menhir_stack _v _menhir_s
  
  and _menhir_run_279 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_typedef -> _ -> _menhir_box_start =
    fun _menhir_stack _v ->
      let MenhirCell1_typedef (_menhir_stack, _menhir_s, td) = _menhir_stack in
      let d = _v in
      let _v = _menhir_action_016 d td in
      _menhir_goto_decsandtys _menhir_stack _v _menhir_s
  
  and _menhir_run_278 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_reldec -> _ -> _menhir_box_start =
    fun _menhir_stack _v ->
      let MenhirCell1_reldec (_menhir_stack, _menhir_s, r) = _menhir_stack in
      let d = _v in
      let _v = _menhir_action_017 d r in
      _menhir_goto_decsandtys _menhir_stack _v _menhir_s
  
  and _menhir_run_277 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_qualdef -> _ -> _menhir_box_start =
    fun _menhir_stack _v ->
      let MenhirCell1_qualdef (_menhir_stack, _menhir_s, q) = _menhir_stack in
      let d = _v in
      let _v = _menhir_action_021 d q in
      _menhir_goto_decsandtys _menhir_stack _v _menhir_s
  
  and _menhir_run_276 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_primdec -> _ -> _menhir_box_start =
    fun _menhir_stack _v ->
      let MenhirCell1_primdec (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let d = _v in
      let _v = _menhir_action_018 d p in
      _menhir_goto_decsandtys _menhir_stack _v _menhir_s
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUALOP ->
              let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ID _v ->
                  _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003
              | _ ->
                  _eRR ())
          | SEMICOLON ->
              let tn = _v in
              let _v = _menhir_action_111 tn in
              _menhir_goto_typedef _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | OF ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState005
          | _ ->
              _eRR ())
      | PIPE | SEMICOLON ->
          let i = _v in
          let _v = _menhir_action_101 i in
          _menhir_goto_tpatmatch _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState007
          | _ ->
              _eRR ())
      | PIPE | SEMICOLON ->
          let typename = _v in
          let _v = _menhir_action_113 typename in
          _menhir_goto_typenameseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_typenameseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState005 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState007 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_009 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let typeargs = _v in
      let _v = _menhir_action_102 i typeargs in
      _menhir_goto_tpatmatch _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_tpatmatch : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_tpatmatch (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012
          | _ ->
              _eRR ())
      | SEMICOLON ->
          let tpm = _v in
          let _v = _menhir_action_104 tpm in
          _menhir_goto_tpatmatchseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_tpatmatchseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState012 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState003 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_013 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_tpatmatch -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_tpatmatch (_menhir_stack, _menhir_s, tpm) = _menhir_stack in
      let tpms = _v in
      let _v = _menhir_action_103 tpm tpms in
      _menhir_goto_tpatmatchseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_010 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_TYPE _menhir_cell0_ID -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell0_ID (_menhir_stack, tn) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
      let cons = _v in
      let _v = _menhir_action_110 cons tn in
      _menhir_goto_typedef _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_typedef : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_typedef (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | RELATION ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | QUAL ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | PRIMITIVE ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | LPAREN ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | ID _v ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState266
      | FORMULA ->
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | ASSUME ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | EOF ->
          let _v = _menhir_action_022 () in
          _menhir_run_279 _menhir_stack _v
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RELATION (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ID _v ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState016
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
          | ID _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState093
          | EQUALOP ->
              let _menhir_s = MenhirState093 in
              let _menhir_stack = MenhirCell1_EQUALOP (_menhir_stack, _menhir_s) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ID _v ->
                  _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v_0 ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState017
      | RPAREN ->
          let i = _v in
          let _v = _menhir_action_041 i in
          _menhir_goto_params _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_params : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState016 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_019 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_params (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState020
      | ID _v ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState020
      | EQUALOP ->
          let _menhir_s = MenhirState020 in
          let _menhir_stack = MenhirCell1_EQUALOP (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState086
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ID _v ->
                  _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState023
              | _ ->
                  _eRR ())
          | ID _v_1 ->
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v_1 in
              let _v = _menhir_action_012 i in
              _menhir_goto_conargs _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | RPAREN ->
              let i = _v in
              let _v = _menhir_action_014 i in
              _menhir_goto_conpat _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025
          | _ ->
              _eRR ())
      | RPAREN ->
          let i = _v in
          let _v = _menhir_action_032 i in
          _menhir_goto_idseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_idseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState023 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState025 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_027 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_ID -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let is = _v in
      let _v = _menhir_action_013 is in
      _menhir_goto_conargs _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_conargs : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let co = _v in
      let _v = _menhir_action_015 co i in
      _menhir_goto_conpat _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_conpat : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_conpat (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUALOP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let t = () in
                  let _v = _menhir_action_024 t in
                  _menhir_run_071_spec_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | LPAREN ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
              | LCURLY ->
                  _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
              | INT _v_1 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let ii = _v_1 in
                  let _v = _menhir_action_023 ii in
                  _menhir_run_071_spec_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | ID _v_3 ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState033
              | FALSE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let f = () in
                  let _v = _menhir_action_025 f in
                  _menhir_run_071_spec_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_071_spec_033 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_conpat -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
  
  and _menhir_run_058 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | UNION ->
          let _menhir_stack = MenhirCell1_ratom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_071_spec_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
          | INT _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_1 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_3 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState059
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | PLUS ->
          let _menhir_stack = MenhirCell1_ratom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_071_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
          | INT _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_6 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_8 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState072
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | MINUS ->
          let _menhir_stack = MenhirCell1_ratom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_071_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
          | INT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_11 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_13 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13 MenhirState074
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | CROSSPRD ->
          let _menhir_stack = MenhirCell1_ratom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_071_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
          | INT _v_16 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_16 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_18 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_18 MenhirState076
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | ARMINUS ->
          let _menhir_stack = MenhirCell1_ratom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_071_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
          | INT _v_21 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_21 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_23 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_23 MenhirState078
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | ANGRIGHT | CONJ | DISJ | EQUALOP | GREATERTHAN | IFF | IMPL | NUMEQ | PIPE | RCURLY | RPAREN | SEMICOLON | SUBSET | SUBSETEQ ->
          let ra = _v in
          let _v = _menhir_action_088 ra in
          _menhir_goto_rexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_071_spec_059 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState059 _tok
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState035 _tok
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState035 _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState035
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState035 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let el = _v in
          let _v = _menhir_action_071 el in
          _menhir_goto_ratom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ARMINUS | CROSSPRD | EQUALOP | GREATERTHAN | MINUS | NUMEQ | PLUS | SUBSET | SUBSETEQ | UNION ->
          let el = _v in
          let _v = _menhir_action_072 el in
          _menhir_goto_ratom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_ratom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _menhir_stack = MenhirCell1_LCURLY (_menhir_stack, _menhir_s) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState037 _tok
          | RPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RCURLY ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_066 () in
                  _menhir_goto_ratom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | INT _v ->
              let _menhir_stack = MenhirCell1_LCURLY (_menhir_stack, _menhir_s) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v in
              let _v = _menhir_action_023 ii in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState037 _tok
          | ID _v ->
              let _menhir_stack = MenhirCell1_LCURLY (_menhir_stack, _menhir_s) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v in
              let _v = _menhir_action_026 i in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState037 _tok
          | FALSE ->
              let _menhir_stack = MenhirCell1_LCURLY (_menhir_stack, _menhir_s) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState037 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_elem (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047 _tok
          | INT _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_1 in
              let _v = _menhir_action_023 ii in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047 _tok
          | ID _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v_3 in
              let _v = _menhir_action_026 i in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047 _tok
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047 _tok
          | _ ->
              _eRR ())
      | RPAREN ->
          let el = _v in
          let _v = _menhir_action_027 el in
          _menhir_goto_elemseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_elemseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState037 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_048 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_elem -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_elem (_menhir_stack, _menhir_s, el) = _menhir_stack in
      let els = _v in
      let _v = _menhir_action_028 el els in
      _menhir_goto_elemseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_043 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LCURLY -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LCURLY (_menhir_stack, _menhir_s) = _menhir_stack in
          let els = _v in
          let _v = _menhir_action_067 els in
          _menhir_goto_ratom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_049 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | ANGRIGHT | ARMINUS | CONJ | CROSSPRD | DISJ | EQUALOP | GREATERTHAN | IFF | IMPL | MINUS | NUMEQ | PIPE | PLUS | RCURLY | RPAREN | SEMICOLON | SUBSET | SUBSETEQ | UNION ->
          let i = _v in
          let _v = _menhir_action_026 i in
          _menhir_goto_elem _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LPAREN ->
          let i = _v in
          let _v = _menhir_action_034 i in
          _menhir_goto_instexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState050
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | RBRACE | STAR ->
          let i = _v in
          let _v = _menhir_action_034 i in
          _menhir_goto_instexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_instexpr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState094 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState256 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState234 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState217 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState149 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState189 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState187 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState182 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_095 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_EQUALOP -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_EQUALOP (_menhir_stack, _) = _menhir_stack in
          let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
          let MenhirCell1_RELATION (_menhir_stack, _menhir_s) = _menhir_stack in
          let ie = _v in
          let _v = _menhir_action_081 i ie in
          _menhir_goto_reldec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_reldec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_reldec (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState268
          | RELATION ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState268
          | QUAL ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState268
          | PRIMITIVE ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState268
          | LPAREN ->
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState268
          | ID _v ->
              _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState268
          | FORMULA ->
              _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState268
          | ASSUME ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState268
          | EOF ->
              let _v = _menhir_action_022 () in
              _menhir_run_278 _menhir_stack _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_QUAL (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REF ->
                  _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState100
              | LBRACE ->
                  _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState100
              | ID _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let t = _v_0 in
                  let _v = _menhir_action_107 t in
                  _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState100 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REF ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | LBRACE ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_107 t in
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RBRACE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v in
              let _v = _menhir_action_108 t in
              _menhir_goto_tyd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_tyd : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState261 ->
          _menhir_run_228_spec_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState252 ->
          _menhir_run_228_spec_252 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState131 ->
          _menhir_run_228_spec_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState247 ->
          _menhir_run_228_spec_247 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState134 ->
          _menhir_run_228_spec_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_228_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState225 ->
          _menhir_run_228_spec_225 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState227 ->
          _menhir_run_228_spec_227 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState144 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_228_spec_261 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ASSUME _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_232_spec_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_232_spec_261 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ASSUME _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let bt = _v in
        _menhir_action_077 bt
      in
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_262 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ASSUME _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_ASSUME (_menhir_stack, _menhir_s) = _menhir_stack in
      let rt = _v in
      let _v = _menhir_action_114 i rt in
      _menhir_goto_typespec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typespec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typespec (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState264
          | RELATION ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState264
          | QUAL ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState264
          | PRIMITIVE ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState264
          | LPAREN ->
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState264
          | ID _v ->
              _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState264
          | FORMULA ->
              _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState264
          | ASSUME ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState264
          | EOF ->
              let _v = _menhir_action_022 () in
              _menhir_run_280 _menhir_stack _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PRIMITIVE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RELATION ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | EQUALOP ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TRUE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let t = () in
                      let _v = _menhir_action_024 t in
                      _menhir_run_071_spec_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | LPAREN ->
                      _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState117
                  | LCURLY ->
                      _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState117
                  | LAMBDA ->
                      _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState117
                  | INT _v_1 ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let ii = _v_1 in
                      let _v = _menhir_action_023 ii in
                      _menhir_run_071_spec_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | ID _v_3 ->
                      _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState117
                  | FALSE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let f = () in
                      let _v = _menhir_action_025 f in
                      _menhir_run_071_spec_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_071_spec_117 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_PRIMITIVE _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState117 _tok
  
  and _menhir_run_118 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let t = () in
                  let _v = _menhir_action_024 t in
                  _menhir_run_071_spec_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | LPAREN ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
              | LCURLY ->
                  _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
              | LAMBDA ->
                  _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
              | INT _v_1 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let ii = _v_1 in
                  let _v = _menhir_action_023 ii in
                  _menhir_run_071_spec_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | ID _v_3 ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState120
              | FALSE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let f = () in
                  let _v = _menhir_action_025 f in
                  _menhir_run_071_spec_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_071_spec_120 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LAMBDA _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState120 _tok
  
  and _menhir_run_124 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState124
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState126
          | _ ->
              _eRR ())
      | RPAREN ->
          let i = _v in
          let _v = _menhir_action_043 i in
          _menhir_goto_paramseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_paramseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState124 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState126 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_128 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_paramseq (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REF ->
                  _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
              | LPAREN ->
                  _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
              | LESSTHAN ->
                  _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
              | LCURLY ->
                  _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
              | LBRACE ->
                  _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
              | ID _v ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState131
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_132 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_s = MenhirState132 in
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REF ->
                  _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
              | LPAREN ->
                  _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
              | LESSTHAN ->
                  _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
              | LCURLY ->
                  _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
              | LBRACE ->
                  _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState134
              | ID _v ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState134
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LESSTHAN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | QUAL ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState135
      | _ ->
          _eRR ()
  
  and _menhir_run_142 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LCURLY (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | REF ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState142
      | LBRACE ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState142
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _menhir_stack = MenhirCell1_ID (_menhir_stack, MenhirState142, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REF ->
                  _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
              | LBRACE ->
                  _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
              | ID _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let t = _v_0 in
                  let _v = _menhir_action_107 t in
                  _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState144 _tok
              | _ ->
                  _eRR ())
          | PIPE | RCURLY ->
              let _v =
                let t = _v in
                _menhir_action_107 t
              in
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_145 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tyd (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_0 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_2 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState146
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ARMINUS | CROSSPRD | EQUALOP | GREATERTHAN | MINUS | NUMEQ | PLUS | RPAREN | SUBSET | SUBSETEQ | UNION ->
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_goto_elem _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | CONJ | DISJ | IFF | IMPL | RCURLY | SEMICOLON ->
          let _v = _menhir_action_049 () in
          _menhir_goto_patom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_elem : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState149 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState256 ->
          _menhir_run_071_spec_256 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState221 ->
          _menhir_run_071_spec_221 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState234 ->
          _menhir_run_071_spec_234 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState217 ->
          _menhir_run_071_spec_217 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState146 ->
          _menhir_run_071_spec_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState148 ->
          _menhir_run_071_spec_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState178 ->
          _menhir_run_071_spec_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState181 ->
          _menhir_run_071_spec_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState206 ->
          _menhir_run_071_spec_206 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState204 ->
          _menhir_run_071_spec_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState202 ->
          _menhir_run_071_spec_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState199 ->
          _menhir_run_071_spec_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState195 ->
          _menhir_run_071_spec_195 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState193 ->
          _menhir_run_071_spec_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState191 ->
          _menhir_run_071_spec_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState189 ->
          _menhir_run_071_spec_189 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState187 ->
          _menhir_run_071_spec_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState182 ->
          _menhir_run_071_spec_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState117 ->
          _menhir_run_071_spec_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState120 ->
          _menhir_run_071_spec_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState084 ->
          _menhir_run_071_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_071_spec_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState078 ->
          _menhir_run_071_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState076 ->
          _menhir_run_071_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState074 ->
          _menhir_run_071_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState072 ->
          _menhir_run_071_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState059 ->
          _menhir_run_071_spec_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState047 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_071_spec_256 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_FORMULA _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState256 _tok
  
  and _menhir_run_071_spec_221 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState221 _tok
  
  and _menhir_run_071_spec_234 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_pred _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_refty -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState234 _tok
  
  and _menhir_run_071_spec_217 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_tyd -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState217 _tok
  
  and _menhir_run_071_spec_146 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_tyd -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState146 _tok
  
  and _menhir_run_071_spec_148 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState148 _tok
  
  and _menhir_run_071_spec_178 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LAMBDA, _menhir_box_start) _menhir_cell1_tybindseq -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState178 _tok
  
  and _menhir_run_071_spec_181 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_EXISTS, _menhir_box_start) _menhir_cell1_tybindseq -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState181 _tok
  
  and _menhir_run_071_spec_206 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState206 _tok
  
  and _menhir_run_071_spec_204 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState204 _tok
  
  and _menhir_run_071_spec_202 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState202 _tok
  
  and _menhir_run_071_spec_199 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState199 _tok
  
  and _menhir_run_071_spec_195 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState195 _tok
  
  and _menhir_run_071_spec_193 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState193 _tok
  
  and _menhir_run_071_spec_191 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState191 _tok
  
  and _menhir_run_071_spec_189 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState189 _tok
  
  and _menhir_run_071_spec_187 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState187 _tok
  
  and _menhir_run_071_spec_182 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ANGLEFT -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState182 _tok
  
  and _menhir_run_071_spec_084 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState084 _tok
  
  and _menhir_run_071_spec_078 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState078 _tok
  
  and _menhir_run_071_spec_076 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076 _tok
  
  and _menhir_run_071_spec_074 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState074 _tok
  
  and _menhir_run_071_spec_072 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let el = _v in
      let _v = _menhir_action_072 el in
      _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072 _tok
  
  and _menhir_goto_patom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState148 ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState256 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState234 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState217 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState149 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_212 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
      let pa = _v in
      let _v = _menhir_action_050 pa in
      _menhir_goto_patom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_198 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IMPL ->
          let _menhir_stack = MenhirCell1_patom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_0 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_2 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState199
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
          | _ ->
              _eRR ())
      | IFF ->
          let _menhir_stack = MenhirCell1_patom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | INT _v_4 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_4 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_6 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState202
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
          | _ ->
              _eRR ())
      | DISJ ->
          let _menhir_stack = MenhirCell1_patom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | INT _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_8 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_10 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10 MenhirState204
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState204
          | _ ->
              _eRR ())
      | CONJ ->
          let _menhir_stack = MenhirCell1_patom (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | INT _v_12 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_12 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_206 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_14 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14 MenhirState206
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_206 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState206
          | _ ->
              _eRR ())
      | RCURLY | RPAREN | SEMICOLON ->
          let pa = _v in
          let _v = _menhir_action_054 pa in
          _menhir_goto_pred _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_148 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | NOT ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | LPAREN ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | LBRACE ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState148
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ANGLEFT ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | _ ->
          _eRR ()
  
  and _menhir_run_149 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | NOT ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LPAREN ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LBRACE ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LAMBDA ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState149 _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState149
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState149 _tok
      | EXISTS ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | ANGLEFT ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | _ ->
          _eRR ()
  
  and _menhir_run_150 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | GREATERTHAN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | INT _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RBRACE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let (rhs, i1) = (_v_0, _v) in
                      let _v = _menhir_action_009 i1 rhs in
                      _menhir_goto_bpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | ID _v_1 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RBRACE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let (i1, i2) = (_v, _v_1) in
                      let _v = _menhir_action_008 i1 i2 in
                      _menhir_goto_bpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | EQUALOP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RBRACE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let i1 = _v in
                      let _v = _menhir_action_006 i1 in
                      _menhir_goto_bpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | STRCONST _v_2 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RBRACE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let (rhs, i1) = (_v_2, _v) in
                      let _v = _menhir_action_011 i1 rhs in
                      _menhir_goto_bpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | INT _v_3 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RBRACE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let (rhs, i1) = (_v_3, _v) in
                      let _v = _menhir_action_010 i1 rhs in
                      _menhir_goto_bpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | ID _v_4 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RBRACE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let (i1, i2) = (_v, _v_4) in
                      let _v = _menhir_action_005 i1 i2 in
                      _menhir_goto_bpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | FALSE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RBRACE ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let i1 = _v in
                      let _v = _menhir_action_007 i1 in
                      _menhir_goto_bpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_bpatom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let ba = _v in
      let _v = _menhir_action_052 ba in
      _menhir_goto_patom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_168 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | _ ->
          _eRR ()
  
  and _menhir_run_169 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REF ->
                  _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
              | LBRACE ->
                  _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState171
              | ID _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let t = _v_0 in
                  let _v = _menhir_action_107 t in
                  _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_172 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_ID (_menhir_stack, v) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_121 ty v in
          (match (_tok : MenhirBasics.token) with
          | COMMA ->
              let _menhir_stack = MenhirCell1_vartybind (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState175
              | _ ->
                  _eRR ())
          | DOT ->
              let vty = _v in
              let _v = _menhir_action_105 vty in
              _menhir_goto_tybindseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_tybindseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState179 ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState168 ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState175 ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_180 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_EXISTS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_tybindseq (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | NOT ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LPAREN ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LBRACE ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LAMBDA ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | INT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v_0 in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v_2 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState181
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EXISTS ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | ANGLEFT ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | _ ->
          _eRR ()
  
  and _menhir_run_179 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EXISTS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState179
      | _ ->
          _eRR ()
  
  and _menhir_run_182 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ANGLEFT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_run_071_spec_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState182
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_177 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LAMBDA as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_tybindseq (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | NOT ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | LPAREN ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | LBRACE ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | LAMBDA ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | INT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v_0 in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v_2 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState178
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_178 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EXISTS ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | ANGLEFT ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState178
      | _ ->
          _eRR ()
  
  and _menhir_run_176 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_vartybind -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_vartybind (_menhir_stack, _menhir_s, vt) = _menhir_stack in
      let vts = _v in
      let _v = _menhir_action_106 vt vts in
      _menhir_goto_tybindseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_pred : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState256 ->
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState234 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState221 ->
          _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState217 ->
          _menhir_run_218 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState146 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState149 ->
          _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState178 ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState206 ->
          _menhir_run_207 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState204 ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState202 ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState199 ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState181 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_257 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_FORMULA _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_FORMULA (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_061 i p in
      let _menhir_stack = MenhirCell1_predspec (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
          | RELATION ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
          | QUAL ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
          | PRIMITIVE ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
          | LPAREN ->
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
          | ID _v ->
              _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState274
          | FORMULA ->
              _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
          | ASSUME ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
          | EOF ->
              let _v = _menhir_action_022 () in
              _menhir_run_275 _menhir_stack _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_251 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REF ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState252
          | LPAREN ->
              _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState252
          | LESSTHAN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState252
          | LCURLY ->
              _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState252
          | LBRACE ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState252
          | ID _v ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState252
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_220 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LCURLY ->
          let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_0 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_221 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_2 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState221
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_221 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState221
          | _ ->
              _eRR ())
      | COMMA | RPAREN | SEMICOLON ->
          let t = _v in
          let _v = _menhir_action_107 t in
          _menhir_goto_tyd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_254 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FORMULA (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUALOP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | NOT ->
                  _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | LPAREN ->
                  _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | LCURLY ->
                  _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | LBRACE ->
                  _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | LAMBDA ->
                  _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | INT _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let ii = _v_0 in
                  let _v = _menhir_action_023 ii in
                  _menhir_run_071_spec_256 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | ID _v_2 ->
                  _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState256
              | FALSE ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let f = () in
                  let _v = _menhir_action_025 f in
                  _menhir_run_071_spec_256 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | EXISTS ->
                  _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | ANGLEFT ->
                  _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState256
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_259 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ASSUME (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | REF ->
                  _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
              | LPAREN ->
                  _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
              | LESSTHAN ->
                  _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
              | LCURLY ->
                  _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
              | LBRACE ->
                  _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
              | ID _v ->
                  _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState261
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_235 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_pred _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_refty -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_refty (_menhir_stack, _, resty) = _menhir_stack in
          let MenhirCell0_ID (_menhir_stack, resvar) = _menhir_stack in
          let MenhirCell1_pred (_menhir_stack, _, pre) = _menhir_stack in
          let MenhirCell1_ID (_menhir_stack, _menhir_s, ef) = _menhir_stack in
          let post = _v in
          let _v = _menhir_action_038 ef post pre resty resvar in
          let mtype = _v in
          let _v = _menhir_action_075 mtype in
          _menhir_goto_refty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_refty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState261 ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState252 ->
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState131 ->
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState247 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState134 ->
          _menhir_run_238 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_237 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState225 ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState227 ->
          _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_253 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let rt = _v in
      let _v = _menhir_action_115 i rt in
      _menhir_goto_typespec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_250 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_paramseq _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_paramseq (_menhir_stack, _, ps) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let rt = _v in
      let _v = _menhir_action_116 i ps rt in
      _menhir_goto_typespec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_248 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_varty, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let rt = _v in
      let _v = _menhir_action_117 i rt in
      _menhir_goto_varty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_varty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_varty (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              let _menhir_s = MenhirState245 in
              let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COLON ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | REF ->
                      _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
                  | LPAREN ->
                      _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
                  | LESSTHAN ->
                      _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
                  | LCURLY ->
                      _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
                  | LBRACE ->
                      _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
                  | ID _v ->
                      _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState247
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | RPAREN ->
          let vt = _v in
          let _v = _menhir_action_122 vt in
          _menhir_goto_vartyseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_vartyseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState245 ->
          _menhir_run_249 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState132 ->
          _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_249 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_varty -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_varty (_menhir_stack, _menhir_s, vt) = _menhir_stack in
      let vts = _v in
      let _v = _menhir_action_123 vt vts in
      _menhir_goto_vartyseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_242 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COMMA | LCURLY | RPAREN | SEMICOLON ->
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let vas = _v in
          let _v = _menhir_action_078 vas in
          _menhir_goto_reftyatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ARROW ->
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let vas = _v in
          let _v = _menhir_action_119 vas in
          _menhir_goto_vartyatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_reftyatom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_goto_refty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_vartyatom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_vartyatom (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REF ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
          | LPAREN ->
              _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
          | LESSTHAN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
          | LCURLY ->
              _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
          | LBRACE ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
          | ID _v ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState227
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_238 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ID (_menhir_stack, _, i) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let fty = _v in
          let _v = _menhir_action_120 fty i in
          _menhir_goto_vartyatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
          let rt = _v in
          let _v = _menhir_action_117 i rt in
          _menhir_goto_varty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_237 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LESSTHAN, _menhir_box_start) _menhir_cell1_parameters -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameters (_menhir_stack, _, ps) = _menhir_stack in
      let MenhirCell1_LESSTHAN (_menhir_stack, _menhir_s) = _menhir_stack in
      let (body, prt) = (_v, ()) in
      let _v = _menhir_action_076 body prt ps in
      _menhir_goto_refty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_233 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_pred _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_refty (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_0 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_234 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_2 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState234
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_234 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState234
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_230 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_vartyatom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_vartyatom (_menhir_stack, _menhir_s, vrta) = _menhir_stack in
      let rt = _v in
      let _v = _menhir_action_074 rt vrta in
      _menhir_goto_refty _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_222 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pred (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              let _menhir_stack = MenhirCell0_ID (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | COLON ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | REF ->
                      _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState225
                  | LPAREN ->
                      _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState225
                  | LESSTHAN ->
                      _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState225
                  | LCURLY ->
                      _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState225
                  | LBRACE ->
                      _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState225
                  | ID _v ->
                      _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState225
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_218 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_tyd -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_tyd (_menhir_stack, _, ty) = _menhir_stack in
          let MenhirCell1_LCURLY (_menhir_stack, _menhir_s) = _menhir_stack in
          let pr = _v in
          let _v = _menhir_action_003 pr ty in
          _menhir_goto_basety _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_basety : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState134 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_232_spec_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState252 ->
          _menhir_run_232_spec_252 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState131 ->
          _menhir_run_232_spec_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState247 ->
          _menhir_run_232_spec_247 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_232_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState225 ->
          _menhir_run_232_spec_225 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState227 ->
          _menhir_run_232_spec_227 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_240 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ID (_menhir_stack, _, i) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let bt = _v in
          let _v = _menhir_action_118 bt i in
          _menhir_goto_vartyatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let bt = _v in
          let _v = _menhir_action_077 bt in
          _menhir_goto_reftyatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_232_spec_252 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let bt = _v in
        _menhir_action_077 bt
      in
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_232_spec_131 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_paramseq _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let bt = _v in
        _menhir_action_077 bt
      in
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_232_spec_247 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_varty, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let bt = _v in
        _menhir_action_077 bt
      in
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_232_spec_141 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LESSTHAN, _menhir_box_start) _menhir_cell1_parameters -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let bt = _v in
        _menhir_action_077 bt
      in
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_run_237 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_232_spec_225 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_pred _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let bt = _v in
        _menhir_action_077 bt
      in
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState225 _tok
  
  and _menhir_run_232_spec_227 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_vartyatom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let bt = _v in
        _menhir_action_077 bt
      in
      let rta = _v in
      let _v = _menhir_action_073 rta in
      _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_213 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_LCURLY, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_tyd -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_tyd (_menhir_stack, _, ty) = _menhir_stack in
          let MenhirCell1_ID (_menhir_stack, _, v) = _menhir_stack in
          let MenhirCell1_LCURLY (_menhir_stack, _menhir_s) = _menhir_stack in
          let pr = _v in
          let _v = _menhir_action_004 pr ty v in
          _menhir_goto_basety _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_210 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let pr = _v in
          let _v = _menhir_action_051 pr in
          _menhir_goto_patom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_208 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LAMBDA, _menhir_box_start) _menhir_cell1_tybindseq -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_tybindseq (_menhir_stack, _, binds) = _menhir_stack in
      let MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) = _menhir_stack in
      let pr = _v in
      let _v = _menhir_action_059 binds pr in
      _menhir_goto_pred _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_207 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_patom (_menhir_stack, _menhir_s, pa) = _menhir_stack in
      let pr = _v in
      let _v = _menhir_action_057 pa pr in
      _menhir_goto_pred _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_205 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_patom (_menhir_stack, _menhir_s, pa) = _menhir_stack in
      let pr = _v in
      let _v = _menhir_action_058 pa pr in
      _menhir_goto_pred _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_203 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_patom (_menhir_stack, _menhir_s, pa) = _menhir_stack in
      let pr = _v in
      let _v = _menhir_action_056 pa pr in
      _menhir_goto_pred _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_200 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_patom (_menhir_stack, _menhir_s, pa) = _menhir_stack in
      let pr = _v in
      let _v = _menhir_action_055 pa pr in
      _menhir_goto_pred _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_197 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_EXISTS, _menhir_box_start) _menhir_cell1_tybindseq -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_tybindseq (_menhir_stack, _, binds) = _menhir_stack in
      let MenhirCell1_EXISTS (_menhir_stack, _menhir_s) = _menhir_stack in
      let pr = _v in
      let _v = _menhir_action_060 binds pr in
      _menhir_goto_pred _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_215 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LCURLY as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RCURLY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LCURLY (_menhir_stack, _menhir_s) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_002 ty in
          _menhir_goto_basety _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PIPE ->
          let _menhir_stack = MenhirCell1_tyd (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | NOT ->
              _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | LPAREN ->
              _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | LBRACE ->
              _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | LAMBDA ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_0 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_217 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_2 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState217
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_217 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | EXISTS ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | ANGLEFT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_127 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let pseq = _v in
      let _v = _menhir_action_044 i pseq in
      _menhir_goto_paramseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_228_spec_252 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_232_spec_252 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_228_spec_131 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_paramseq _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_232_spec_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_228_spec_247 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_varty, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_232_spec_247 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_228_spec_134 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState134 _tok
  
  and _menhir_run_228_spec_141 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LESSTHAN, _menhir_box_start) _menhir_cell1_parameters -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_232_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_228_spec_225 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_ID, _menhir_box_start) _menhir_cell1_pred _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_232_spec_225 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_228_spec_227 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_vartyatom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let ty = _v in
      let _v = _menhir_action_001 ty in
      _menhir_run_232_spec_227 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_109 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_tyd as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COLONARROW ->
          let _menhir_stack = MenhirCell1_tyd (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REF ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
          | LBRACE ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
          | ID _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_0 in
              let _v = _menhir_action_107 t in
              _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState110 _tok
          | _ ->
              _eRR ())
      | COMMA | GREATERTHAN | SEMICOLON ->
          let s = _v in
          let _v = _menhir_action_096 s in
          _menhir_goto_sortseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_sortseq : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_tyd as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState108 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState110 ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_112 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_QUAL _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_tyd -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_tyd (_menhir_stack, _, s) = _menhir_stack in
      let ss = _v in
      let _v = _menhir_action_095 s ss in
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_QUAL (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_065 i p in
      _menhir_goto_qualdef _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_qualdef : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState264 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState266 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState274 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState272 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState270 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState268 ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState135 ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_269 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_qualdef (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState270
          | RELATION ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState270
          | QUAL ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState270
          | PRIMITIVE ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState270
          | LPAREN ->
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState270
          | ID _v ->
              _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState270
          | FORMULA ->
              _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState270
          | ASSUME ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState270
          | EOF ->
              let _v = _menhir_action_022 () in
              _menhir_run_277 _menhir_stack _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_136 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_qualdef (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | QUAL ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState137
          | _ ->
              _eRR ())
      | GREATERTHAN ->
          let p = _v in
          let _v = _menhir_action_039 p in
          _menhir_goto_parameters _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_parameters : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState135 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState137 ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_139 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LESSTHAN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_parameters (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REF ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
          | LPAREN ->
              _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
          | LESSTHAN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
          | LCURLY ->
              _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
          | LBRACE ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
          | ID _v ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_138 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_qualdef -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_qualdef (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ps = _v in
      let _v = _menhir_action_040 p ps in
      _menhir_goto_parameters _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_111 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_tyd, _menhir_box_start) _menhir_cell1_tyd -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_tyd (_menhir_stack, _menhir_s, s) = _menhir_stack in
      let ss = _v in
      let _v = _menhir_action_097 s ss in
      _menhir_goto_sortseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_107 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_QUAL _menhir_cell0_ID as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_tyd (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLONARROW ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | REF ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
          | LBRACE ->
              _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
          | ID _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_0 in
              let _v = _menhir_action_107 t in
              _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState108 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_106 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_REF (_menhir_stack, _menhir_s) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_109 t in
      _menhir_goto_tyd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_087 : type  ttv_stack. (((ttv_stack, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_params, _menhir_box_start) _menhir_cell1_EQUALOP -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_EQUALOP (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_params (_menhir_stack, _, p) = _menhir_stack in
          let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
          let MenhirCell1_RELATION (_menhir_stack, _menhir_s) = _menhir_stack in
          let ie = _v in
          let _v = _menhir_action_082 i ie p in
          _menhir_goto_reldec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RPAREN ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let (i, ie) = (_v_0, _v) in
                  let _v = _menhir_action_069 i ie in
                  _menhir_goto_ratom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | COMMA ->
                  let _menhir_stack = MenhirCell1_instexpr (_menhir_stack, _menhir_s, _v) in
                  let (_menhir_s, i) = (MenhirState062, _v_0) in
                  let _v = _menhir_action_029 i in
                  _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_funparam (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let i = _v_0 in
              let _v = _menhir_action_029 i in
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState068 _tok
          | _ ->
              _eRR ())
      | RPAREN ->
          let p = _v in
          let _v = _menhir_action_030 p in
          _menhir_goto_funparams _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_funparams : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState068 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState062 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_070 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_funparam -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_funparam (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ps = _v in
      let _v = _menhir_action_031 p ps in
      _menhir_goto_funparams _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_instexpr -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_instexpr (_menhir_stack, _menhir_s, ie) = _menhir_stack in
      let ps = _v in
      let _v = _menhir_action_068 ie ps in
      _menhir_goto_ratom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_053 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LBRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              let _menhir_stack = MenhirCell1_instexpr (_menhir_stack, _menhir_s, _v) in
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState054
          | LPAREN | RBRACE | STAR ->
              let MenhirCell1_LBRACE (_menhir_stack, _menhir_s) = _menhir_stack in
              let ie = _v in
              let _v = _menhir_action_036 ie in
              _menhir_goto_instexprs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_instexprs : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState054 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState049 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LBRACE, _menhir_box_start) _menhir_cell1_instexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_instexpr (_menhir_stack, _, ie) = _menhir_stack in
      let MenhirCell1_LBRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let ies = _v in
      let _v = _menhir_action_037 ie ies in
      _menhir_goto_instexprs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_052 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let ies = _v in
      let _v = _menhir_action_035 i ies in
      _menhir_goto_instexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_rexpr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState149 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState195 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState193 ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState191 ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState189 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState187 ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState256 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState234 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState217 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState178 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState204 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState182 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState117 ->
          _menhir_run_121_spec_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState120 ->
          _menhir_run_121_spec_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState076 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState074 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState072 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState059 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState035 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_209 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_rexpr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUBSETEQ ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUBSET ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NUMEQ ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATERTHAN ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALOP ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_187 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_run_071_spec_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState187
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState187
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState187
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_189 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_run_071_spec_189 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState189
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState189
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_189 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState189
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_189 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_rexpr (_menhir_stack, _, re) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_070 re in
      _menhir_goto_ratom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_191 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_run_071_spec_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState191
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_193 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_run_071_spec_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState193
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState193
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_195 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = () in
          let _v = _menhir_action_024 t in
          _menhir_run_071_spec_195 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState195
      | LCURLY ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState195
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let ii = _v in
          let _v = _menhir_action_023 ii in
          _menhir_run_071_spec_195 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ID _v ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState195
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let f = () in
          let _v = _menhir_action_025 f in
          _menhir_run_071_spec_195 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_196 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_rexpr (_menhir_stack, _menhir_s, re1) = _menhir_stack in
      let re2 = _v in
      let _v = _menhir_action_090 re1 re2 in
      _menhir_goto_rpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_rpatom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let ra = _v in
      let _v = _menhir_action_053 ra in
      _menhir_goto_patom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_194 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_rexpr (_menhir_stack, _menhir_s, re1) = _menhir_stack in
      let re2 = _v in
      let _v = _menhir_action_094 re1 re2 in
      _menhir_goto_rpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_192 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_rexpr (_menhir_stack, _menhir_s, re1) = _menhir_stack in
      let re2 = _v in
      let _v = _menhir_action_093 re1 re2 in
      _menhir_goto_rpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_190 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_rexpr (_menhir_stack, _menhir_s, re1) = _menhir_stack in
      let re2 = _v in
      let _v = _menhir_action_091 re1 re2 in
      _menhir_goto_rpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_188 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_rexpr -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_rexpr (_menhir_stack, _menhir_s, re1) = _menhir_stack in
      let re2 = _v in
      let _v = _menhir_action_092 re1 re2 in
      _menhir_goto_rpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_186 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_rexpr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUBSETEQ ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SUBSET ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NUMEQ ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GREATERTHAN ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUALOP ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_183 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ANGLEFT -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | ANGRIGHT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ANGLEFT (_menhir_stack, _menhir_s) = _menhir_stack in
          let re = _v in
          let _v = _menhir_action_089 re in
          _menhir_goto_rpatom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_121_spec_117 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_PRIMITIVE _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let re = _v in
      let _v = _menhir_action_063 re in
      _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_123 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_PRIMITIVE _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_PRIMITIVE (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_062 i p in
      let _menhir_stack = MenhirCell1_primdec (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState272
          | RELATION ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState272
          | QUAL ->
              _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState272
          | PRIMITIVE ->
              _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState272
          | LPAREN ->
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState272
          | ID _v ->
              _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState272
          | FORMULA ->
              _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState272
          | ASSUME ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState272
          | EOF ->
              let _v = _menhir_action_022 () in
              _menhir_run_276 _menhir_stack _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_121_spec_120 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LAMBDA _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let re = _v in
      let _v = _menhir_action_063 re in
      _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_122 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LAMBDA _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_LAMBDA (_menhir_stack, _menhir_s) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_064 i p in
      _menhir_goto_primdef _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_primdef : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState117 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState120 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let re = _v in
      let _v = _menhir_action_046 i re in
      _menhir_goto_patmatch _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_patmatch : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PIPE ->
          let _menhir_stack = MenhirCell1_patmatch (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
          | ID _v ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState091
          | _ ->
              _eRR ())
      | SEMICOLON ->
          let pm = _v in
          let _v = _menhir_action_048 pm in
          _menhir_goto_patmatchseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_ID (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EQUALOP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = () in
              let _v = _menhir_action_024 t in
              _menhir_run_071_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | LPAREN ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
          | LCURLY ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
          | INT _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let ii = _v_1 in
              let _v = _menhir_action_023 ii in
              _menhir_run_071_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ID _v_3 ->
              _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState084
          | FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let f = () in
              let _v = _menhir_action_025 f in
              _menhir_run_071_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_patmatchseq : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState093 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState020 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_097 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_RELATION (_menhir_stack, _menhir_s) = _menhir_stack in
      let patmseq = _v in
      let _v = _menhir_action_079 i patmseq in
      _menhir_goto_reldec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_092 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_patmatch -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_patmatch (_menhir_stack, _menhir_s, pm) = _menhir_stack in
      let pms = _v in
      let _v = _menhir_action_047 pm pms in
      _menhir_goto_patmatchseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_089 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_RELATION _menhir_cell0_ID, _menhir_box_start) _menhir_cell1_params -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_params (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell0_ID (_menhir_stack, i) = _menhir_stack in
      let MenhirCell1_RELATION (_menhir_stack, _menhir_s) = _menhir_stack in
      let patmseq = _v in
      let _v = _menhir_action_080 i p patmseq in
      _menhir_goto_reldec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN _menhir_cell0_conpat -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_conpat (_menhir_stack, cp) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let re = _v in
      let _v = _menhir_action_045 cp re in
      _menhir_goto_patmatch _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ratom (_menhir_stack, _menhir_s, ra) = _menhir_stack in
      let re = _v in
      let _v = _menhir_action_087 ra re in
      _menhir_goto_rexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_077 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ratom (_menhir_stack, _menhir_s, ra) = _menhir_stack in
      let re = _v in
      let _v = _menhir_action_083 ra re in
      _menhir_goto_rexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_075 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ratom (_menhir_stack, _menhir_s, ra) = _menhir_stack in
      let re = _v in
      let _v = _menhir_action_085 ra re in
      _menhir_goto_rexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ratom (_menhir_stack, _menhir_s, ra) = _menhir_stack in
      let re = _v in
      let _v = _menhir_action_086 ra re in
      _menhir_goto_rexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_060 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ratom -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ratom (_menhir_stack, _menhir_s, ra) = _menhir_stack in
      let re = _v in
      let _v = _menhir_action_084 ra re in
      _menhir_goto_rexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_056 : type  ttv_stack. ((ttv_stack, _menhir_box_start) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_start) _menhir_state -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_rexpr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let is = _v in
      let _v = _menhir_action_033 i is in
      _menhir_goto_idseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_018 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, i) = _menhir_stack in
      let p = _v in
      let _v = _menhir_action_042 i p in
      _menhir_goto_params _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_008 : type  ttv_stack. (ttv_stack, _menhir_box_start) _menhir_cell1_ID -> _ -> _ -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ID (_menhir_stack, _menhir_s, typename) = _menhir_stack in
      let tnseq = _v in
      let _v = _menhir_action_112 tnseq typename in
      _menhir_goto_typenameseq _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let rec _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_start =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | RELATION ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | QUAL ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | PRIMITIVE ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | LPAREN ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | ID _v ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | FORMULA ->
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_100 () in
          MenhirBox_start _v
      | ASSUME ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | _ ->
          _eRR ()
  
end

let start =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_start v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
