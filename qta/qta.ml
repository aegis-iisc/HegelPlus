(* Main definition for the Qualified Tree Automata *)



open SpecLang
module Set = Stdlib.Set
exception QTAException of string     
exception QTAUnimplemented 

module RefTy = SpecLang.RefinementType
module Syn = Lambdasyn
module SynTC = Syntypechecker
module Similarity = Orderedequivalence
module QTACons = QTACons
module RunMap = RunMap
module Gamma = Environment.TypingEnv 

open Definitions


module Symbol = struct 

    (* a symbol name with an arity *)
    type t = (Var.t * int)

end     

module Transition = struct
  
    type t = (symbol * (State.t list) * QTACons.t * State.t)



end



(* The symbols with arity and the Alphabet *)


 
type alphabet = (Symbol.t) list

 

 (* qtaconstraint *)

 (* transition *)
(* type transition = (symbol * (State.t list) * QTACons.t * State.t) *)


  (* val subterm *)



  (* A type for QTA*)
  type t = QTA of {q: State.t list;
                   f : alphabet;
                   qf : State.t list;
                   delta : transition list}


  let emptyQTA = raise QTAUnimplemented              
  (* Run of an automata, takes a qta and a term and retruns a map 
  mapping each position in t to a state in qta *)

  let rec run (qta : t) (term : Syn.monExp) : (RunMap.t) = 
      rause QTAUnimplemented


  let is_accepting (rho : RunMap.t) : bool = 
      raise QTAUnimplemented
      
      

    (* The semantics function 
    This must be defined in terms of the nodes and transitions
    like we had in the previous submission
    J𝑞K𝑞 ::= Ð𝑖 (J𝐸𝑖 K𝑒 )
where 𝐸𝑖 =
(𝑓 (𝑞𝑖 1, 𝑞𝑖 2,. . . ,𝑞𝑖𝑛 ) ⇝ 𝑞)
J𝐸K𝑒 ::= { 𝑓 𝑡𝑖 | 𝑡𝑖 ∈ J𝑞𝑖 K𝑞 , 𝑖 ∈ [1 . . . 𝑛] }
where 𝐸 =
(𝑓 (𝑞1, 𝑞2,. . . ,𝑞𝑛 ) ⇝ 𝑞)
JAK ::= Ð𝑖 {J𝑞𝑖 K𝑞 | 𝑞𝑖 ∈ 𝑄𝑓 }
Env(A) ::= {(x : 𝜏) | 𝑓 (𝑞𝑖 ) ⇝ [x : 𝜏] ∈ Δ}
    
    
    *)
    let rec language (qta : t) : (Syn.monExp list) = 
      raise QTAUnimplemented    



    (* For both union and intersection, the states will be a product state set, 
    but for each of those we will simply define a new state *)
    let rec union (qta1 : t) (qta2 : t) : t = 
        raise QTAUnimplemented


    let rec sem_intersection (qta1 : t) (qta2 : t) : t = 
        raise QTAUnimplemented



    
    let rec sem_intersection_mod_const (qta1 : t) (cons : QTACons.t) (qta2 : t) : t = 
        raise QTAUnimplemented
        




    (* Similar to the below definition from the Previous paper 
    Env(A) ::= {(x : 𝜏) | 𝑓 (𝑞𝑖 ) ⇝ [x : 𝜏] ∈ Δ  
    Basically with every transition, we now keep the type at the position type - i.e. position 0*)

     let rec environment (qta : t) : Gamma.t = 
            raise QTAUnimplemented




    
    (* The contraints comparing two transitions for Subtyping relations *)
    let rec sub (d1 : Transition.t) (d2 : Transition.t) : QTACons.t = 
            raise QTAUnimplemented        


(* we can simply check the sucess by traversing the tree and checking if there is a term with a type
      subtupe of the query.
    Ideally this should be defined using the emptyness check *)
let rec successful_run (a : t) (query : RefTy.t) : State.t list =
    
        raise QTAUnimplemented

(* A function collecting all the terms accepted by a tree automat *)
let rec collecting_semantics (a : t) : (Syn.monExp list) = 
        raise QTAUnimplemented   


(* Reduction using semantic intersection, calling it Typed reduction 
The logic hoes here*)
let rec typed_reduction (a : t)  : t  = 
    raise QTAUnimplemented




let rec extract_similarities (a : t) :  Similarity.equivalence = 
    raise QTAUnimplemented


let 

(* Similarity reduction algorithm logic will go here 
This cmprises both of similarity edge finding as well as minimization*)
let rec similarity_red (a : t) : t = 
        raise QTAUnimplemented