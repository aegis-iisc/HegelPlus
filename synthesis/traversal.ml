open SpecLang 
module PTA = Pta.PTAutomata
exception TraversalException of string  
module FBSMAP = BookKeeping.FBS




module Message = struct 

  let show (s:string) = Printf.printf "%s" ("\n ")
  let debug (s:string) = Printf.printf "%s" ("\n "^s)
  (*define other utilities*)
  
end


open PTA
module Util = struct


(*simplest backtracking algorithm which randomly choses on of the predecessor 
  while keeping a list of already visited state_vars
*)
let rec backtrackPTA (a : t) (f : frontier) (backtracked : FBSMAP.t) : 
        (t * frontier) option = 
  match f with 
      | Bottom -> None 
      | Term s ->  
                 (* get one of the previous element previous element *)
                 let delta_incoming =  get_transitions a s  in 

                 Message.debug ("Delta Incomings "^(string_for_list 
                                          (List.map (fun di -> string_for_delta di) delta_incoming)));
                 match delta_incoming with 
                   | [] -> raise (TraversalException "Backtracking from a non-bottom must have a delta")
                   | di :: _ -> 
                      let (symbol_d, qis, _) = di in 
                      let (fname, _) = symbol_d in 
                      (*pick one qi out of quis at random*)
                      (*
                        This case does not arise now as we have a special state BottomState 
                        if (List.length qis = 0) then 
                        let backtrack_list_4f = 
                          try    
                            FBSMAP.find backtracked f 
                          with
                            | _ -> [] 
                        in      
                        let bottom_state_var = PTA.bottom_state_var () in 
                        if (List.exists (fun bt_vari -> 
                              PTA.equal_state_var bt_vari bottom_state_var) backtrack_list_4f) then 
                                None 
                        else 
                          Some (a, Bottom) 
                      else 
                        *)  
                        let backtrack_list_f =  
                            try    
                              FBSMAP.find backtracked f 
                            with
                              | _ -> [] 
                        in      
                        let non_backtracked = List.filter (fun qi ->
                                                    let (_,svar_qi,_,_) = qi in 
                                                    not (List.exists 
                                                      (fun bt_vari -> PTA.equal_state_var bt_vari svar_qi) backtrack_list_f
                                                    ))  
                                                    qis in 
                          match non_backtracked with 
                            | [] -> None 
                            | _ ->  let new_frontier = List.hd (Pta.rand_select qis 1) in 
                                    let (_,_,_,level) = new_frontier in 
                                    (* we return Bottom or non-bottom  *)
                                    if (level > 0) then 
                                      Some (a, Term new_frontier)
                                    else
                                      Some (a, Bottom) 







end 
