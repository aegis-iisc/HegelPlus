open SpecLang 
module PTA = Pta.PTAutomata
module Gamma = Environment.TypingEnv
exception NoMappingForFrontier of string
exception IllegalConstructorType of string  


(* A map from the frontier to a list of trnasitions *)
(* Frontier-Transitions Map *)
module FTM = struct 

  module FTMKey =
    struct
      type t = PTA.frontier
      let equal(t1,t2)  =  
        match (t1, t2) with 
         | (PTA.Bottom, PTA.Bottom) -> true
         | (PTA.Term s1, PTA.Term s2) -> 
                (*the let-bind variable is unique*)
                let (_,v1,_,_) = s1 in 
                let (_,v2,_,_) = s2 in 
                Var.equal v1 v2 
                (* if (((PTA.State.compare s1 s2) == -1) &&
                  ((PTA.State.compare s2 s1) == 1)) then true
                else  *)
                  (* false *)
          | (_, _) -> false
    end
  
  module FTMValue  =
    struct 
      (*Need to change later to scehema*)
      type t = Var.t list
      let rec equal (t1,t2) = 
          match (t1, t2) with 
            | ([],[]) -> true
            | (x :: xs, y :: ys) -> (Var.equal x y) && (equal (xs, ys))   
            | (_, _) -> false     
  
  end
  
  module FTMap   = Applicativemap.ApplicativeMap (FTMKey) (FTMValue) 

    type t = FTMap.t
    let empty = FTMap.empty
    let mem = FTMap.mem
    let find t k = 
        try (FTMap.find t k) 
      with 
      | (FTMap.KeyNotFound k) -> raise (NoMappingForFrontier (PTA.string_for_frontier k))
    
    let add = fun t -> fun var rt -> FTMap.add t var rt
    let remove = FTMap.remove
    let append t binds = 
            t@binds
    let update (gamma : Gamma.t) t k (d : Var.t) : t = 
         try 
           let current_value = find t k in 
           let new_value = current_value @ [d] in 
           add t k new_value 
         with
          | NoMappingForFrontier _ -> add t k [d]    

    let toString t = 
        
        List.fold_left (fun accstr (k, v) -> 
            let value_str = List.fold_left (fun accvi vi -> accvi^(" : ")^(Var.toString vi)) "Value " v in 

            (accstr^"\n "^(PTA.string_for_frontier k)^" \\mapsto "^(value_str))) " " t 
    

  
  
  end
  
(* We also need a map for backtracking in BFS, 
   Frontier -> [State_var] 
   A special state_var named bottom_bottom state, with type
   *)
module FBS = struct
 module FBSMKey =
   struct
     type t = PTA.frontier
     let equal(t1,t2)  =  
       match (t1, t2) with 
        | (PTA.Bottom, PTA.Bottom) -> true
        | (PTA.Term s1, PTA.Term s2) -> 
               (*the let-bind variable is unique*)
               let (_,v1,_,_) = s1 in 
               let (_,v2,_,_) = s2 in 
               Var.equal v1 v2 
               (* if (((PTA.State.compare s1 s2) == -1) &&
                 ((PTA.State.compare s2 s1) == 1)) then true
               else  *)
                 (* false *)
         | (_, _) -> false
   end
 
 module FBSValue  =
   struct 
     (*Need to change later to scehema*)
     type t = PTA.state_var list
     
     let rec equal (t1,t2) = 
      match (t1, t2) with 
        | ([],[]) -> true
        | (x :: xs, y :: ys) -> 
            (PTA.equal_state_var x y) && (equal (xs, ys))   
        | (_, _) -> false     
 end
 
 module FBSMap  = Applicativemap.ApplicativeMap (FBSMKey) (FBSValue) 
  type t = FBSMap.t
  let empty = FBSMap.empty
  let mem = FBSMap.mem
  let find t k = 
     try (FBSMap.find t k) 
   with 
   | (FBSMap.KeyNotFound k) -> raise (NoMappingForFrontier (PTA.string_for_frontier k))
 
  let add = fun t -> fun var rt -> FBSMap.add t var rt
  let remove = FBSMap.remove
  let append t binds = 
         t@binds
  let update t k (d : PTA.state_var) : t = 
      try 
        let current_value = find t k in 
        let new_value = current_value @ [d] in 
        add t k new_value 
      with
       | NoMappingForFrontier _ -> add t k [d]    
  let toString t = 
     "FBSMAP Abstract"
     (* List.fold_left (fun accstr (vi, rti) -> (accstr^"\n "^(Var.toString vi)^" : "^(RefTy.toString rti))) " " t  *)
 


 end
