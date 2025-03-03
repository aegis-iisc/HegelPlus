module A = Pta
open SpecLang
module Sigma = Environment.Constructors 
module Gamma = Environment.TypingEnv
module P = Predicate 
exception GeneratorExc of string  
exception Unimplemented 
(* USe the OCaml Graph library https://github.com/backtracking/ocamlgraph *)
(* Top level Prudent application *)

type environment = (Gamma.t * Sigma.t)


type ioexample = (string list * string)
(* A query refined with i/o examples *)
type ioquery = IOGoal of { 
                        library : environment;
                        psi : TyD.t;
                        refinements : ioexample
                        }

type query = Goal of {library : environment;
                        psi : RefTy.t}


let generate givenquery : (ioquery list) = 
    raise Unimplemented


