exception DefException of string     
exception DefUnimpl of string

module RefTy = SpecLang.RefinementType
module Syn = Lambdasyn

let state_count = ref 0 

(* StateNames, we need a lot of these *)
module State = struct

  (* State is same as the name of the state along with a tag if is is final*)
  type t =  (Ident.t * bool)
  let toString ((name, tag) : t) : string = 
         "("^(Ident.name name)^", "^(string_of_bool tag)^")"
  
  
  
  let base = "Svar_"


  let fresh_statename s = 
    let id = base^(s)^ string_of_int !state_count in 
    let _ = state_count := !state_count + 1
    in 
    (Ident.create id, false) (*default state is not final*)

  let equal t1 t2 = Ident.equal t1 t2
end

module Position = struct
  (* A psotion is N*, i.e. list of Natural Numbers *)
  type t = int list
  let toString = raise DefUnimpl

  let equal t1 t2 = raise DefUnimpl


  
end

module QTACons = struct

(* We also need to assign a label to each position, 
the main idea is to extract type for each transition *)
let position_label = raise DefUnimpl



(* Language of QTA Constraints  *)
type t = SynEq of position * position 
        | SymEq of position * position 
        | Neg of t 
        | Conj of t * t 
        | Disj of t * t

type schema = SSynEq | SSymEq


(* val constraint_schema *)
let constraint_schema = raise DefUnimpl 


end