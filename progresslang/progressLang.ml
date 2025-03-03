(* This language goes out for now
Currently, we do not    
*)

open SpecLang
open Z3
exception IllegalProgressExp of string 
module P = Predicate
module Relation = 
    struct 



    end 

module SMTModel = 
    struct 
    type t = M of Z3.Model.model 
            | U 

    end    

module Constant = 
    struct 
    type t = 
          True 
        | False 
        | IVal of int 
        | Empty 
    end

module SMTResult = 
    struct 
    type t =      
             SMTTrue of RefTy.t 
            | SMTFail of SMTModel.t     
                

    let getModel t = 
        match t with 
            | SMTFail m -> m 
            | SMTTrue _ -> raise (IllegalProgressExp "No model for successful") 


    let getType t =            
        match t with 
            | SMTTrue ty -> ty 
            | SMTFail _ -> raise (IllegalProgressExp "No model for successful") 


    end

(* language_for_defining_progress_predicates
    these will always be of the form 
*)
        
        
(* variable := x, y, z...
rel := < | > | == | \in  relations are ranged over Set<a>, Int, Bool)
constants :=  v | True | false | i | s |    
atoms = constant | variable

ppred := 
  | mpred (\overline {v}) 
  | atom rel atom 
  | ppred /\ ppred 
  | ppred \/ ppred
  | not ppred 
*)
module ProgressTerm = 
    struct 
    type variable = Var.t 

    type atom = 
        C of Constant.t
        | V of variable



    type t = 
        | True 
        | False 
        (* | PRel of atom * atom * Relation.t  *)
        | PConj of t * t 
        | PDisj of t * t
        | PNeg of t
        | PMethod of RelId.t * Var.t list

    let atomToString atom = 
        "Printing utility for atomString"


    let ppredicateToString t = 
        "Printing utility for ppredicate"


    let init () = 
        True    
        

    (* 
       get the model
       get_unsatisfiabiity_core
       get 
    
     *)
    (* let synthesizeCorePredicate (res : SMTResult.t) : t = 
         let model =  res.getModel 
 *)

    end


