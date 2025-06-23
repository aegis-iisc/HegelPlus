(* Implementation for the similarity rules *)





open SpecLang
module Set = Stdlib.Set
exception QTAException of string     
exception SimUnimplemented 

module RefTy = SpecLang.RefinementType
module Syn = Lambdasyn
module SynTC = Syntypechecker
module QTA = Qta
module QTACons = QTACons
module RunMap = RunMap
module Gamma = Environment.TypingEnv 
module Defs = Definitions


module OEq = Orderedequivalence

module TransEquivalence = OEq.OrderededEquivalence (QTA.Transition)   

type equivalence = TransEquivalence.t 




let sim_trans (a : QTA.t) (d1 : QTA.trans) (d2 : QTA.trans) : bool = raise SimUnimplemented
 
let sim_eq (a : QTA.t) 
             (e : equivalence) 
             (d1 : QTA.trans) 
             (d2 : QTA.trans) : equilavence = raise SimUnimplemented




