(* Module implementing the QTASynthesis algorithm in the paper *)
module Syn = Lambdasyn
open SpecLang
module SynTC = Syntypechecker
module Gamma = Environment.TypingEnv
module Sigma = Environment.Constructors
module P = Predicate
module QTA = Qta
module SMTRes = ProgressLang.SMTResult
module VC = VerificationC   
module VCE = Vcencode 
module Trans = QTA.Transition
exception PTASynthesis of string 
exception Unimplemented


type result = None | Some of (Syn.monExp list)


(* check if the transition is already visited *)
let visited (visited: Trans.t list) (f : Trans.t) : bool = 
  (* get the set of tramsitions visited from the frontier *)
    let transition_label Trans.get_label (f) in 
    List.exists (fun ti -> if 
        ((Trans.get_label vi) == transition_label) then true else false) visited                    



let qta_synthesis (library : Gamma.t) (query :  RefTy.t) (k : int) : result = 
    (* create an empty gamma and a with a single state *)
    let (a_0, gamma_0) = Trans.trans_init library in 
    
    (* we can simply check the sucess by traversing the tree and checking if there is a term with a type
      subtupe of the query.
    Ideally this should be defined using the emptyness check *)
    let success = QTA.successful_run a_0 query in 
    match success with 
      | [] -> qta_enumerate a_0 library query k 
      | _::_ -> Some (QTA.collecting_semantics (a_0))  


let rec qta_enumerate (a : QTA.t) (library : Gamma.t) (query :  RefTy.t) (k : int) : result = 
    let d = QTA.depth (a) in 
    (* we start with an initial gamma same as the library *)
    let g = library in 
    let (a, g) = Trans.trans_secondary a library g in 
    (* reducing infeasible terms using symbolic intersection *)
    let a_red = QTA.typed_reduction a  in 
    let a_sim = QTA.similarity_red a_red in 
    let g_red = QTA.environmet a_sim in
    
    if (d < k) then 
        let (a, g, _) = Trans. in 
        
    else
