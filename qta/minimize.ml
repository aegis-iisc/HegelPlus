(* implementation for the minimization rules *)





open SpecLang
module Set = Stdlib.Set
exception QTAException of string     
exception MinUnimplemented 

module RefTy = SpecLang.RefinementType
module Syn = Lambdasyn
module SynTC = Syntypechecker
module QTA = Qta
module QTACons = QTACons
module RunMap = RunMap
module Gamma = Environment.TypingEnv 
module Defs = Definitions



(* M-Trand Rule *)
let m_trans (a : QTA.t) (e : Similarity.equivalence) : (QTA.t * Similarity.equilavence) = 
    raise MinUnimplemented


(* ( A, E ) ⊢ Δ ⇝★ Δ′ *)
let m_trans_star (a : QTA.t) (e : Similarity.equivalence) : (QTA.t * Similarity.equilavence) = 
  raise MinUnimplemented

let m_qta (a : QTA.t) (e : Similarity.equivalence) : (QTA.t * Similarity.equilavence) = 
  raise MinUnimplemented


