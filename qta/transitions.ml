(* Add the addition of transitions for wellformedness and expressions *)





open SpecLang
module Set = Stdlib.Set
exception QTAException of string     
exception TransUnimplemented 

module RefTy = SpecLang.RefinementType
module Syn = Lambdasyn
module SynTC = Syntypechecker
module QTA = Qta
module Similarity = Orderedequivalence
module QTACons = QTACons
module RunMap = RunMap
module Gamma = Environment.TypingEnv 
module Defs = Definitions



  (* expression rule in the paper
    returns a new \Gamma and a new QTA with the addition transitions
    Plus added transition *)
let expression (a : QTA.t) (gamma : Gamma.t) (e : Syn.monExp) : (QTA.t  * Gamma.t * QTA.transition) = 

  match e with 
    | Evar v 
    | Elam (args, body)  
    | Eapp (f, args) 
    | Ematch (m, cases) 
    | Elet ( x, e1, e2) 
    | Ecapp (c, args)  
    | Eite (cexp, et, ef) -> raise TransUnimplemented
   


(* adds transitions corresponding to all primitive types from the library *)
let wff_prim_types 
              (a : QTA.t) 
              (gamma : Gamma.t) 
              : (QTA.t  * Gamma.t * QTA.transition) =  
              raise TransUnimplemented


(*All wf formulas  *)
let wff_formulas 
              (a : QTA.t) 
              (gamma : Gamma.t) 
              : (QTA.t  * Gamma.t * QTA.transition) =  
              raise TransUnimplemented

(*All refinement types in the library  *)
let wff_base_ref  (a : QTA.t) 
                  (gamma : Gamma.t) 
                  : (QTA.t  * Gamma.t * QTA.transition) =  
                  raise TransUnimplemented




(*All arrow refinement  *)
let wff_arrow_ref (a : QTA.t) 
                  (gamma : Gamma.t) 
                  : (QTA.t  * Gamma.t * QTA.transition) =  
                  raise TransUnimplemented             



(* Initial transitions, wff, Evar and Econst *)
let trans_init
(a : QTA.t)
(library : Gamma.t) 
(gamma : Gamma.t) : (QTA.t  * Gamma.t) =  
        raise TransUnimplemented            



(* transitions for function applications, conditionals and Type constructors, applies
constructing a closure *)
let trans_secondary
(a : QTA.t)
(library : Gamma.t) 
(gamma : Gamma.t)
: (QTA.t  * Gamma.t) =  
        raise TransUnimplemented            






