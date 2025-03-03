

module Syn = Lambdasyn
open SpecLang
module SynTC = Syntypechecker
module Gamma = Environment.TypingEnv
module Sigma = Environment.Constructors
module P = Predicate
module PTA = Pta.PTAutomata

module PPTerm = ProgressLang.ProgressTerm
module SMTRes = ProgressLang.SMTResult
module VC = VerificationC   
module VCE = Vcencode 
module FTMAP = BookKeeping.FTM 
module FBSMAP = BookKeeping.FBS
module Traversal = Traversal.Util
exception StaticPTAExc of string 
exception Unimplemented
(* let gamma = ref (Gamma.empty : Gamma.t) *)
let qualifiers = ref []
let max_depth = ref 0 


module Message = struct 

  let show (s:string) = Printf.printf "%s" ("\n ")
  let debug (s:string) = Printf.printf "%s" ("\n "^s)
  (*define other utilities*)

end




let rec exploreGamma g potential_choices frontier frontier_tyd : (Var.t * int)list = 
    (match g with
    | [] -> potential_choices
    | (vi, rti) :: xs -> 
        if (String.equal (Var.toString vi) "goal") then 
            let _ = Message.show ("################################################") in 
            let _ = Message.show ("Skipping Variable "^(Var.toString vi)^" As no \ 
                                recursive call support for now") in 
            let _ = Message.show ("################################################") in 
            
            exploreGamma xs potential_choices frontier frontier_tyd
        else
         (match rti with 
            | RefTy.Arrow ((_, _), _) -> 
                let _ = Message.debug ("Rank :: Arrow "^(Var.toString vi)^" :: "^(RefTy.toString rti)) in 
                (match (RefinementType.uncurry_Arrow rti) with 
                    |   RefinementType.Uncurried (args_types_list, ret_type) ->
                        let arity_vi = List.length args_types_list in 
                        if (List.exists (fun (ai, ati) -> 
                            let base_ati = RefinementType.toTyD ati in 
                            Message.debug (" Component type "^(TyD.toString base_ati));
                            Message.debug (" Frontier type "^(TyD.toString frontier_tyd));
                            
                            TyD.sametype base_ati frontier_tyd  
                            ) args_types_list) then 
                            (exploreGamma xs ((vi, arity_vi) :: potential_choices)) frontier frontier_tyd 
                        else (*could not find any arguments with matching base type with the frontier*)
                            exploreGamma xs  potential_choices frontier frontier_tyd (*No argument has a feasible choice*)
                    | _ -> raise (StaticPTAExc "Uncurried format must be RefTy.Uncurried")        
                )    
            | _ ->   
             exploreGamma xs potential_choices frontier frontier_tyd 
           
         )    
    )


(*  Rank the set function in the gamma 
    based on the avilable arguments in PTA and the aruments 
    base types
    If there are no arguments in the pta, then that choice can be ignored
*)
let rank  (a : PTA.t)
         (f : PTA.state) 
         (gamma : Gamma.t) :  (Var.t * int) list  = 
         
    let (_,_,ri,_) = f in 
    let frontier_tyd = 
        (match ri with 
                    | PTA.First rty -> RefinementType.toTyD rty
                    | PTA.Higher k -> TyD.Ty_star  
        )  
    in 
    Message.debug ("Ranking at Frontier "^(PTA.string_for_state f));
    Message.debug ("PTA "^(PTA.string_for_pta a));
    let states = PTA.getStates a in
    assert(List.length states > 0); 
    exploreGamma gamma [] f frontier_tyd          




let bfsBase (a : PTA.t) 
            (args_states : PTA.state list) 
                    : PTA.t = 
    Message.debug ("bfsBase Addiition first level of transtions bot -> arg_i");      
    let PTA {q;f;qf;delta;edges}  = a in 

    (*The base BFS step*)
    let new_transitions = List.map (fun qi -> 
                                let (gi, svi, sti, li ) = qi in 
                                let refti = match sti with 
                                    | PTA.First rti -> rti
                                    | PTA.Higher _  -> RefTy.Star
                                in
                                let symbol_i = (PTA.State.getStateVar qi, RefTy.arity refti) in 
                                let sources_i = [PTA.bottom_state ()] in 
                                (symbol_i, sources_i, qi)
                                ) args_states in


    PTA.PTA {q=q;f=f;qf=qf;delta=new_transitions;edges}

let enumerate   (gamma : Gamma.t) 
    (quals :  SpecLang.Qualifier.t list) 
    (query : RefinementType.t) 
    (maxPathLength : int) : 
   (PTA.t * string * Syn.monExp list) = 


    let uncurried_goal = RefTy.uncurry_Arrow query in 

    let RefTy.Uncurried (typed_args, typed_body) = uncurried_goal in 
    

    let (gamma_extended, init_args_states) = List.fold_left (fun (acc_gamma,acc_qlist) (vi, ti) -> 

                                            let qi = 
                                                match ti with 
                                                    | RefTy.Star -> raise (Unimplemented)
                                                    | _ ->    (acc_gamma, (PTA.Exp vi), (PTA.First ti), 1) in 
                                            let acc_gamma = ((vi, ti)::gamma) in
                                            let acc_qlist = (qi:: acc_qlist) in  
                                            (acc_gamma, acc_qlist))  (gamma,[]) typed_args in 


    (* A symbolic level 1000 is treated as the top level  *)
    let final_state = (gamma_extended, PTA.top_state_var (), PTA.top_state_type typed_body, 1000) in 

    let symbols = List.map (fun (vi, ti) -> 
                            let arity_ti = RefinementType.arity ti in 
                            (vi , arity_ti)
                        ) gamma in 


    (*Initial QTA with just bottom and final states*)
    let pta_bottom = PTA.PTA {q = [PTA.bottom_state ()]@init_args_states@[final_state];
                            f = symbols;
                            qf = [final_state];
                            delta = [];
                            edges = []} in 

    let pta_base = bfsBase pta_bottom init_args_states in 


    Message.debug (" The PTA after adding arguments "^(PTA.string_for_pta pta_base));
    
    let rec bfsProgress a level gamma qualifiers max_level : (PTA.t) = 
        if (level = max_level) then a
        else 
            let states_at_level = PTA.getState_for_i a level  in 
            
            (* A function adding 
              all possible 
              transition at a given frontier state *)
            let add_transitions_at_frontier (gamma : Gamma.t) 
                                            (frontier : PTA.state) : 
                                            (PTA.t * PTA.transition list) = 
                let choices = rank a frontier gamma  in 
                let rec loop_choices choices (updated_pta : PTA.t) (accumlator : PTA.transition list)
                                                 : (PTA.t * PTA.transition list) = 
                    (match choices with 
                         [] ->  (updated_pta, accumlator) 
                        | ci :: ci_xs -> 
                            Message.debug (" Trying the next choice");
                            let expandedPTA = 
                                PTA.addEdgeAndTransition a (Term frontier) 
                                                        (level) gamma 
                                                        !qualifiers ci in
                            (*  *)
                            (match expandedPTA with 
                                | None -> 
                                    loop_choices ci_xs updated_pta accumlator 
                                | Some (a_new,d_new,t_new) -> 
                                    let _ = Message.debug ("Found the next Choice ") in 
                                    (* Check if we have reached the goal *)
                                    (match t_new with 
                                      | Bottom -> raise (StaticPTAExc "Expansion cannot see a Bottom")
                                      | Term s_new -> 
                                          let PTA {q;f;qf; delta; edges} = a_new in 
                                          let (g_new,_,_,_) = s_new in 
                                           let v' = PTA.State.getStateVar s_new in 
                                           let rt' = PTA.State.getStateType s_new in 
                                           Message.debug ("************** Potential  Target Node "^(RefTy.toString rt'));
                                           (*if the base type do not match then it disrupt the typechecking*) 
                                           let base_rt' = RefTy.toTyD rt' in   
                                           let base_goal = RefTy.toTyD typed_body in 
                                           
                                          (if (TyD.sametype base_rt' base_goal) then 
                                              let vc = VC.fromTypeCheck gamma [] (rt', typed_body) in 
                                              let vcStandard = VC.standardize vc in 
                                              let (result) = VCE.discharge vcStandard [] !qualifiers  in 
                                              let typechecks = 
                                                match result with 
                                                | VCE.Success -> true
                                                | VCE.Failure -> false
                                                | VCE.Undef -> false
                                              (* raise (SynthesisException "Typechecking Did not terminate")   *)
                                              in 
                                              if (typechecks) then 
                                                 let _ =  Message.debug ("DFS Progress found a PTA Node satisfying the Goal, Running Denotation Now") in 
                                                 raise (StaticPTAExc "Found a solution, stopping as Denotation function is currently buggy")
                                                 
                                              else  
                                                 loop_choices ci_xs (a_new) (d_new :: accumlator)
                                          else
                                                 loop_choices ci_xs (a_new) (d_new :: accumlator)        
                                          )  
                                    )
                            )
                        )      
                in 
                (loop_choices choices (a) [])
            (*add_transition_body_ends *)
            in


            (*folding over all the states at a level*)
            let ((new_automata, new_transitions) : PTA.t * PTA.transition list) = 
                    List.fold_left (fun accumulator frontier_i  -> 
                        add_transitions_at_frontier gamma frontier_i 
                    ) (a, []) states_at_level 
            in 
           
            (*recursively call *)
            bfsProgress new_automata (level + 1) gamma qualifiers max_level

    in 
    Message.debug ("Calling bfsProgress");
    let (pta_complete) = bfsProgress pta_base 1 gamma qualifiers maxPathLength in 

    Message.debug ("Returned");
    
    let (outstring, synth_terms) = PTA.walk_n_search pta_complete query in 
    (pta_complete, outstring, synth_terms)



