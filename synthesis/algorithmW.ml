(*
This module defines the whole algorithm where we learn the 
core failure predicate and the progress predicate
 *)


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
exception PTASynthesis of string 
exception Unimplemented
(* let gamma = ref (Gamma.empty : Gamma.t) *)
let qualifiers = ref []
let max_depth = ref 0 

let minimization = ref true
let lca = ref true 



(* a contains all the available nodes,
   gamma contains all the future functions
   How can we make this dependent on the goal.
   Rank function just returns a list of component names with the arity
   We can optimize this by using some kind of static checking by checking if we have some 
    node in the A for each argument for the component
   *)

module Message = struct 

    let show (s:string) = Printf.printf "%s" ("\n ")
    let debug (s:string) = Printf.printf "%s" ("\n "^s)
    (*define other utilities*)
    
end

let visited (visitor : FTMAP.t) (f : PTA.frontier) (transition_label : Var.t ) : bool = 
    (* get the set of tramsitions visited from the frontier *)
    let visited_set = try 
                        FTMAP.find visitor f 
                       with
                        | _ -> [] 
                    in                     
    List.exists (fun vi -> if (vi == transition_label) then true else false) visited_set                    

let rec exploreGamma g potential_choices frontier frontier_tyd visitor : (Var.t * int)list = 
    
    match g with
    | [] -> potential_choices
    | (vi, rti) :: xs -> 
        if (String.equal (Var.toString vi) "goal") then 
            let _ = Message.show ("################################################") in 
            let _ = Message.show ("Skipping Variable "^(Var.toString vi)^" As no \ 
                                recursive call support for now") in 
            let _ = Message.show ("################################################") in 
            
            exploreGamma xs potential_choices frontier frontier_tyd visitor
        (* Already visited from this frontier *)
        else if (visited visitor frontier vi) then 
            let _ = Message.show ("################################################") in 
            let _ = Message.show ("Skipping Variable "^(Var.toString vi)^" As already visited from the frontier") in 
            let _ = Message.show ("################################################") in 
            (* raise (PTASynthesis "Forced stop after visited is true"); *)
            exploreGamma xs potential_choices frontier frontier_tyd visitor
        else
         (match rti with 
            (*TArrow case*)
            | RefTy.Arrow ((_, _), _) -> 
                let _ = Message.debug ("Rank :: Arrow "^(Var.toString vi)^" :: "^(RefTy.toString rti)) in 
                let uncurried = RefinementType.uncurry_Arrow rti in 
                let RefinementType.Uncurried (args_types_list, ret_type) = uncurried in 
                let arity_vi = List.length args_types_list in 
                (* 
                1. if given the component type xi : ti -> {...}, \exists some ti, for which 
                the current frontier has a macthing base type.
                2. select all such components as high ranked or feasible choices to chose from 
                3. This can be later optimized to give higher rank to components which have higher 
                number of arguments feasible in the QTA
                *)
               
               
                (* let args_with_fav_states = List.filter (fun (ai, ati) -> 
                         let base_ati = RefinementType.toTyD ati in 
                         if (List.mem (base_ati) tyd_list) then 
                            true 
                         else false) args_types_list in  *)
                if (List.exists (fun (ai, ati) -> 
                    let base_ati = RefinementType.toTyD ati in 
                    Message.debug (" Component type "^(TyD.toString base_ati));
                    Message.debug (" Frontier type "^(TyD.toString frontier_tyd));
                    
                    TyD.sametype base_ati frontier_tyd  
                    ) args_types_list) then 
                    (exploreGamma xs ((vi, arity_vi) :: potential_choices)) frontier frontier_tyd visitor     
                else (*could not find any arguments with matching base type with the frontier*)
                    exploreGamma xs  potential_choices frontier frontier_tyd visitor (*No argument has a feasible choice*)
            | RefTy.Base (_,_,_) -> 
                let _ = Message.show ("Rank :: Base "^(Var.toString vi)^" :: "^(RefTy.toString rti)) in 
                (* Non-arrow types not interesting for CBS *)
                exploreGamma xs potential_choices frontier frontier_tyd visitor
                (* exploreGamma xs ((vi, 0) :: potential_choices)A base type is a  0 arity function        *)
            | _ -> raise (PTASynthesis ("Ranking a given type is not allowed, may be its better to skip if this is fired "^(RefTy.toString rti)))
     
         )    

(*  Rank the set function in the gamma 
    based on the avilable arguments in PTA.
    If there are no arguments in the pta, then that choice can be ignored
*)
let rank a 
         f 
         gamma 
         visitor : (Var.t * int) list  = 

    match f with 
       | PTA.Bottom -> 
            (*get the list of input args*)
            let goal  =
                
                try 
                    Gamma.find gamma "goal"
                with
                    | _ -> raise (PTASynthesis "No goal in Environment") 
            in 
            Message.debug ("Goal Found "^(RefTy.toString goal));



            let args_state = PTA.getState_for_i a 1 in 
            let non_visited = List.filter (fun qi -> 
                let vi = PTA.State.getStateVar qi in 
                not (visited visitor f vi)
                ) args_state in 
            (* The arity for each of the arguments will be 0 *)
            let choices_with_arity = List.map (fun qi -> 
                let vi = PTA.State.getStateVar qi in 
                (vi, 0)          
                ) non_visited in 
            Message.debug ("Number of possible args "^(string_of_int(List.length choices_with_arity)));
            choices_with_arity

            (* raise (PTASynthesis "FORCED"); *)

       | PTA.Term s ->  
            let (_,_,ri,_) = s in 
            let frontier_tyd = (match ri with 
                    | PTA.First rty -> RefinementType.toTyD rty
                    | PTA.Higher k -> TyD.Ty_star  
                )  in 
            Message.debug ("Ranking at Frontier "^(PTA.string_for_frontier f));
            Message.debug ("PTA "^(PTA.string_for_pta a));
            let states = PTA.getStates a in
            assert (List.length states > 0); 
            exploreGamma gamma [] f frontier_tyd visitor          


(* Lets see if we need seperate functions from bot and non bot cases *)
let rec find_next_state_from_bot (choices : PTA.state list) 
                                 (visitor : FTMAP.t) : (PTA.state option) = 
    let visited = 
        try    
            FTMAP.find visitor (PTA.Bottom) 
        with
            | _ -> [] 
    in     
    let rec loop list =                          
        (match list with 
            | [] -> None
            | x :: xs -> 
                (* if vi is not already visited from Bottom choose and return it  *)
                let v_x = PTA.State.getStateVar x in 
                if (List.exists (fun vi ->  Var.equal vi v_x) visited) then
                    loop xs 
                else 
                    Some (x)  
        )            
    in 
    loop choices
               
               
(* Choose the next-comoponent at a frontier t in a QTA a with 
   frontier t at a level i
   Add the next component using a new transition, PTA.addEdgeAndTransition*)
let rec r_choice (potential_choioces : PTA.symbol list)
                 (a : PTA.t) 
                (t : PTA.frontier) 
                (level : int)
                (gamma : Gamma.t)
                (visitor : FTMAP.t) : (FTMAP.t * (PTA.t * PTA.transition * PTA.frontier) option) =  
    match potential_choioces with 
        | [] ->  (visitor, None)
        | ci :: cxs -> 
                Message.debug ("****************TRYING CHOICE***************"^(Var.toString (fst (ci))));
                (* keep track of visited  (succeeded/failed components, TODO, this may
                   need to be decoupled into two distinct maps later *)
                let (var, arity) = ci in 
                (* Adding an edge will require 
                    finding the correct component 
                    and all its arguments 
                *)
                (* 
                With higher-order functions, 
                now we also work with partial application 
                make a choice of the transition,
                continue expansion if this choice succeeds else 
                make a different choice  
                *)
                Message.show (" Before Adding Edge and Transitions "^(PTA.string_for_pta a));
                (*Message.show (" Before Frontier "^(PTA.string_for_frontier t));
                When we add the new node and transition, we should add hyperedges   
                *)
                let expandedPTA = PTA.addEdgeAndTransition a t (level) gamma !qualifiers ci in
                (match expandedPTA with 
                    | None -> 
                        r_choice cxs a t level gamma visitor (* Case when the current choice did not work*)
                    | Some (a',d', t') -> 
                        let _ = Message.debug ("Found the next Choice ") in 
                        Message.debug (" Updating frontier map for "^(PTA.string_for_frontier t));
                        let visitor = FTMAP.update gamma visitor t var in 
                         Message.debug (" After Adding Edge and Transitions "^(PTA.string_for_pta a'));
                        (visitor, Some (a', d', t'))
                )
               
(*Unify the design for bottom and non bottom case
   If the frontier is Bottom, then explore the choices at level 0, which are query args, add the corresponding transitions to the 
   PTA and update the frontier
   Else find function calls and do the same
   Implements the current version of the algorithm, modulo the beam width*)    
let rec dfsProgress (a : PTA.t) 
                    (t : PTA.frontier) 
                    (gamma : Gamma.t)
                    (backtracked : FBSMAP.t)
                    (visitor : FTMAP.t)
                    (goal : RefinementType.t) : (PTA.t * FTMAP.t * Syn.monExp list) = 
        (* rank sorted list of components
           cis must be a list of symbol = (Var.t * Syn.monExp.t * arity) *)
        (*currently there is no ranking algorithm*)
        Message.debug (" DFS Progress Called, minimizing the automaton before progressing");
        Message.debug (" Automata in DFS before minimization "^(PTA.string_for_pta a));
        Message.debug (" Frontier "^(PTA.string_for_frontier t));
        

        let PTA.PTA {q; f; qf; delta; edges} = a in 
        match t with 
            | PTA.Bottom -> 
                    let states_at_level_1 = PTA.getState_for_i a 1 in 
                    let bot_state = PTA.bottom_state () in  
                    Message.debug ("****************TRYING CHOICE in BOTTOM CASE***************");
                    (* Find a next state which has not been visited  *)
                    
                    (*𝑐 𝑖 ← random_select (𝑐𝑠𝑖 )
                       This is a random selection from all the possible choices at this point *)       
                    
                    
                    (* The random choice, or rather the first non-visited choice *)
                    let chosen = find_next_state_from_bot states_at_level_1 visitor in 
                    (* 6) if ∀𝑐𝑖 ∈ 𝑐𝑖 . 𝑐𝑖 = ⊥ then
                        (7) BFSEnumerate (A, 𝑞𝑖 , L, Ψ)
                        else
                        (8) (A’, 𝑞𝑖 ’) ← AddTransitions (A, 𝑞𝑖 , 𝑐𝑖 , b)
                        (9) A” ← AddSimilarity (A’, 𝑞𝑖 ’)
                        (10) (Amin, 𝑞𝑖 min) ← Minimize (A”);
                        (11) res ← CheckSMT (Amin, 𝑞𝑖 min, Ψ)
                        (12) if (res ≠ ⊥) then return JAminK ;
                        (13) else if not max_depth then
                        (14) DFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ);
                        (15) else BFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ)  *)
                    (match chosen with 
                        | None -> (*No more choices*)
                            Message.debug ("****************NO NEXT CHOICE in BOTTOM START BFSProgress **************");
                            (* Nothing to backtrack her so probably no need for bfsProgress *)
                            bfsProgress a t gamma backtracked visitor goal (*No further choice so backtrack and search in bfs*)
                        | Some s ->
                                (* 
                                (A’, 𝑞𝑖 ’) ← AddTransitions (A, 𝑞𝑖 , 𝑐𝑖 , b))
                                (create a new transition  
                                bot ---> {x, []}---> s
                                *)
                               let (g, _, _, l) = s in
                               Message.debug ("****************FOUND A CHOICE ***************"^(PTA.string_for_state s));
                    
                               
                               let v_var = PTA.State.getStateVar s in 
                               let r_type = PTA.State.getStateType s in 
                               let new_transition = ((v_var,0), [], s) in 
                               

                               let a' = 
                                 PTA.PTA {
                                 q= q; 
                                 f=f; 
                                 qf=qf; 
                                 delta = (new_transition :: delta); 
                                 edges = edges} in 

                                Message.debug ("****************Added the Chosen Transition***************"^(PTA.string_for_delta new_transition));
                     
                                (* see if this node is equivalent to some other noder in the tree and
                                add hyper edges in the tree
                                Whendver we add a new node and edge in the tree we add corresponsing hyperedges.
                                *)
                                (* (9) A” ← AddSimilarity (A’, 𝑞𝑖 ’) *)
                                Message.debug ("****************Finding New Similarities***************");
                     
                                let updated_hedges = PTA.add_hyper_edges a s in 

                           

                                let a' = 
                                 PTA.PTA {
                                 q= q; 
                                 f=f; 
                                 qf=qf; 
                                 delta = delta; 
                                 edges = updated_hedges} in 

                                
                                Message.debug (" Updating frontier map for "^(PTA.string_for_frontier t));
                                let visitor = FTMAP.update gamma visitor t v_var in 
                                let t' = PTA.Term s in 

                               (* Check if reached the query goal or recurse *)
                                Message.debug ("****************Bottom Case: NEXT CHOICE Found Check if Goal is Recahed ************** ");
                                Message.debug ("************** Potential  Target Node Type "^(RefTy.toString (r_type)));
                                (*if the base type do not match then it disrupt the typechecking*) 
                                let base_r = RefTy.toTyD r_type in   
                                let base_goal = RefTy.toTyD goal in 
                                if (TyD.sametype base_r base_goal) then (*Base type match*)
                                   let vc = VC.fromTypeCheck gamma [] (r_type, goal) in 
                                   let vcStandard = VC.standardize vc in 
                                   let (result) = VCE.discharge vcStandard [] !qualifiers  in 
                                   let typechecks = 
                                     match result with 
                                     | VCE.Success -> true
                                     | VCE.Failure -> false
                                     | VCE.Undef -> false
                                   
                                   in 
                                   if (typechecks) then 
                                      let _ =  Message.debug ("DFS Progress found a 
                                            PTA Node satisfying the Goal, Running Denotation Now") in 
                                      (* Create a term from the Tree Automaton and the Frontier *)
                                      (a', visitor, PTA.denotation_node a' t')
                                   else  (* miles to go before your sleep
                                        else if not max_depth then
                                        (14) DFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ);
                                    (15) else BFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ
                                        *)
                                       let _ =  Message.debug ("DFS Progress, Goal not reached call dfs again") in 
                                      
                                    (* Go deeper if the max depth is not yet reached, else backtrack and do bfs *)
                                       if (PTA.get_frontier_level t' <= !max_depth) then 
                                           dfsProgress a' t' gamma backtracked visitor goal
                                       else 
                                           let _ = Message.debug ("MAX DEPTH OF THE TREE REACHED TRY BFS") in 
                                           bfsProgress a' t' gamma backtracked visitor goal
                                else (* base type mismatch so keep going*)
                                   let _ = Message.show ("Non matching base type "^(TyD.toString base_r)^" != "^(TyD.toString base_goal)) in 
                                   if (PTA.get_frontier_level t' <= !max_depth) then 
                                       dfsProgress a' t' gamma backtracked visitor goal
                                   else 
                                       let _ = Message.debug ("MAX DEPTH OF THE TREE REACHED CASE 2") in 
                                        bfsProgress a' t' gamma backtracked visitor goal      
                    )                     
                                    

                    
            | PTA.Term (_,_,_,level) ->  (*The general case of adding a new transition in a DFS with b=1*)
                (*This should take the Library the current automata A and find a ranking*)    
                (* R_Rank *)
                let cis = rank a t gamma visitor in 
                Message.debug ("****************RANKING DONE in DFS with Non Bottom Frontier 
                                ****Found # of components "^(string_of_int (List.length cis)));
                Message.debug ("****************LOOKING FOR NEXT CHOICE TRANSITION AND NODE***************");
                (* if ∀𝑐𝑖 ∈ 𝑐𝑖 . 𝑐𝑖 = ⊥ then
                        (7) BFSEnumerate (A, 𝑞𝑖 , L, Ψ)
                        else
                        (8) (A’, 𝑞𝑖 ’) ← AddTransitions (A, 𝑞𝑖 , 𝑐𝑖 , b) *)
                let (visitor, chosen) = r_choice cis a t level gamma visitor in 
                (* 6)   
                        (9) A” ← AddSimilarity (A’, 𝑞𝑖 ’)
                        (10) (Amin, 𝑞𝑖 min) ← Minimize (A”);
                        (11) res ← CheckSMT (Amin, 𝑞𝑖 min, Ψ)
                        (12) if (res ≠ ⊥) then return JAminK ;
                        (13) else if not max_depth then
                        (14) DFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ);
                        (15) else BFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ)  *)
                match (chosen) with 
                    | None ->   
                            Message.debug ("****************NO NEXT CHOICE START BFSProgress **************");
                            bfsProgress a t gamma backtracked visitor goal (*No further choice so backtrack and search in bfs*)
                    | Some (a', d', t') -> (*Keep going*)
                         Message.debug ("****************NEXT CHOICE Found Check if Goal is Recahed ************** ");
                         Message.debug ("**************** The current Frontier **************"^(PTA.string_for_frontier t'));
                            
                         (match t' with 
                            | Term s' -> 

                                let PTA {q;f;qf; delta; edges} = a' in 
                                (* If minimization is true, add hyperedges and minimize*)
                                let (a_min, t_min) = 
                                (if (!minimization) then  
                                    (*(9) A” ← AddSimilarity (A’, 𝑞𝑖 ’)  *)
                                     let updated_hedges = PTA.add_hyper_edges a' s' in 
                                        (* update the current automata *)
                                        let a' = 
                                         PTA.PTA {
                                      q= q; 
                                      f=f; 
                                      qf=qf; 
                                      delta = delta; 
                                      edges = updated_hedges} in 
                                      (*if minimization is needed
                                        (10) (Amin, 𝑞𝑖 min) ← Minimize (A”); 
                                       *)

                                     PTA.minimize a' t' 
                                else 
                                    (a', t')
                                )     
                                in 
                                let s_min = (match t_min with 
                                        | Term s' -> s'
                                        | Bottom -> raise (PTASynthesis "Minimization matched a frontier to a Bottom")
                                 ) in  
                                (* 
                                 (11) res ← CheckSMT (Amin, 𝑞𝑖 min, Ψ)
                                  *)
                                 let (g',_,_,_) = s_min in 
                                 let v' = PTA.State.getStateVar s_min in 
                                 let rt' = PTA.State.getStateType s_min in 
                                 Message.debug ("************** Potential  Target Node "^(RefTy.toString rt'));
                                 (*if the base type do not match then it disrupt the typechecking*) 
                                 let base_rt' = RefTy.toTyD rt' in   
                                 let base_goal = RefTy.toTyD goal in 
                                 
                                if (TyD.sametype base_rt' base_goal) then 
                                    let vc = VC.fromTypeCheck gamma [] (rt', goal) in 
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
                                        (* (12) if (res ≠ ⊥) then return [[Amin]]  *)
                                       let _ =  Message.debug ("DFS Progress found a PTA Node satisfying the Goal, Running Denotation Now") in 
                                       raise (PTASynthesis "Found a solution, stopping as Denotation function is currently buggy")
                                       (a_min, visitor, PTA.denotation_node a_min t_min)
                                    else  
                                        (* 
                                        (13) else if not max_depth then
                                        (14) DFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ); *)
                                        (* Go deeper if the max depth is not yet reached, else backtrack and do bfs *)
                                        if (PTA.get_frontier_level t_min <= !max_depth) then 
                                            dfsProgress a_min t_min gamma backtracked visitor goal
                                        else 
                                            let _ = Message.debug ("MAX DEPTH OF THE TREE REACHED TRY BFS") in 
                                            bfsProgress a_min t_min gamma backtracked visitor goal
                                else 
                                    (* Base types did not match so, no need to further check *)
                                    let _ = Message.show ("Non matching base type "^(TyD.toString base_rt')^" != "^(TyD.toString base_goal)) in 
                                    if (PTA.get_frontier_level t_min <= !max_depth) then 
                                        dfsProgress a_min t_min gamma backtracked visitor goal
                                    else 
                                        let _ = Message.debug ("MAX DEPTH OF THE TREE REACHED CASE 2") in 
                                        bfsProgress a_min t_min gamma backtracked visitor goal      
                            | Bottom -> dfsProgress a' t' gamma backtracked visitor goal    
                         )            
                                

and bfsProgress (a : PTA.t) 
                (t : PTA.frontier) 
                (gamma : Gamma.t)
                (backtracked : FBSMAP.t)
                (visitor : FTMAP.t ) (**Keep a list of trnsitions at frontier, and skip visiting the transitions *)
                (* (theta : PPTerm.ppredicate) *)
                (goal : RefinementType.t) : (PTA.t * FTMAP.t * Syn.monExp list)  = 
    let _ = Message.debug ("Starting BFS PROGRESS") in 
    Message.show (" A at BFS "^(PTA.string_for_pta a));  
    Message.debug ("Lets Backtrack: Backtracking From frontier "^(PTA.string_for_frontier t));
     (* Two different cases for backtracking 
        1.  the frontier is already bottom then nothing to backtrack 
        2. Other wise backtrack to a level lower*)
     (*TODO ??The bug is in backtrack*)
    (match t with 
        | Bottom ->
            (*Nothing to backtrack*)
            (a, visitor, []) 
        | Term _ ->   
            (* else
            (30) 𝑐𝑠𝑖 ← R_Rank (A, 𝑞𝑖 , L, Ψ)
            (31) 𝑐𝑖 ← random_select (𝑐𝑠𝑖 )
            (32) if ∀𝑐𝑖 ∈ 𝑐𝑖 . 𝑐𝑖 = ⊥ then
                (33) BFSEnumerate (A, 𝑞𝑖 , L, Ψ)
            else
                (34) (Amin, 𝑞min) ← Minimize (A′)
                (35) DFSEnumerate (Amin, 𝑞min, L, Ψ) *)
            let previous = Traversal.backtrackPTA a t backtracked in 
            (match previous with 
                | None -> (*Nothing new to backtrack*)
                    let _ = Message.debug ("Exhausted all backtracking choices") in 
                    (a, visitor, []) 
                | Some (a', t') ->  
                    Message.debug ("Backtracked Automata "^(PTA.string_for_pta a'));
                    Message.debug ("Backtracked frontier "^(PTA.string_for_frontier t'));
                    (match t' with 
                        | Bottom -> 
                            let _ = Message.debug ("BACKTRACKED TO BOTTOM FRONTIER") in  
                            let backtracked = FBSMAP.update backtracked t (PTA.Exp (Var.fromString "BottomState"))  in 
                            let cis = rank a' t' gamma visitor in 
                            let _ = Message.debug ("RANKING DONE # Choices "^(string_of_int (List.length cis))) in 
                           (match cis with 
                               | [] -> 
                                    (* if ∀𝑐𝑖 ∈ 𝑐𝑖 . 𝑐𝑖 = ⊥ then
                                    (33) BFSEnumerate (A, 𝑞𝑖 , L, Ψ) *)
                                     bfsProgress a t gamma backtracked visitor goal
                               | _ -> 
                                    (*At bottomfrontier, we just choice from one of the transitions
                                      corresponding to a argument*)
                                    let args_state = PTA.getState_for_i a 1 in 
                                    Message.debug ("Frontier Map "^(FTMAP.toString visitor));
                                    let choice = find_next_state_from_bot args_state visitor in 
                                    match choice with 
                                        | None -> (* if ∀𝑐𝑖 ∈ 𝑐𝑖 . 𝑐𝑖 = ⊥ then
                                            (33) BFSEnumerate (A, 𝑞𝑖 , L, Ψ) *)
                                             bfsProgress a t gamma backtracked visitor goal
                                        | Some s ->      
                                            let s_var = PTA.State.getStateVar s in     
                                             (* (A’, 𝑞𝑖 ’) ← AddTransitions (A, 𝑞𝑖 , 𝑐𝑖 , b) *)
                                             (*We skip adding the new transition as 
                                                the transitions to all arguments from bottom 
                                                already added in the initialization*)
                                            let (g, _, _, l) = s in
                                            Message.debug ("****************FOUND the next frontier ***************"^(PTA.string_for_state s));
                                            let a' = a in
                                            (* (9) A” ← AddSimilarity (A’, 𝑞𝑖 ’) *)
                                            Message.debug ("****************Finding Similarities***************");
                                            let (a_min, t_min) = 
                                            (if (!minimization) then 
                                                 let updated_hedges = PTA.add_hyper_edges a s in 
                                                 let PTA.PTA {q;f;qf;delta;edges} = a' in 
                                                 let a' = 
                                                  PTA.PTA {
                                                  q= q; 
                                                  f=f; 
                                                  qf=qf; 
                                                  delta = delta; 
                                                  edges = updated_hedges} in 

                                                 Message.debug (" Updating frontier visitor map FTMAP for "^(PTA.string_for_frontier t));
                                                 let visitor = FTMAP.update gamma visitor t' s_var in 
                                                 (* Update the  new frontier *)
                                                 let t' = PTA.Term s in 

                                                 let _ = Message.debug ("Minimizing the Automata") in 
                                                 let (a', t') =  PTA.minimize a' t' in 
                                                 (a', t') 
                                            else
                                                (a', t) 
                                            )
                                            in 
                                            Message.debug ("Minimizing Done ");
                                            (* Call the dfsprogress to move further *)
                                            dfsProgress a_min t_min gamma backtracked visitor goal
                            )        
                        | Term s' -> 
                            let (_,sv', st', s'_level) = s' in 
                            let _ = Message.debug ("BACKTRACKED TO Non Bottom Frontier "^(PTA.string_for_frontier t')) in  
                            let backtracked = FBSMAP.update backtracked t (sv')  in 
                            
                            let _ = Message.debug ("RANKING ALL THE COMPONENTS") in 
                            let cis = rank a' t' gamma visitor in 
                            let _ = Message.debug ("RANKING DONE # Choices "^(string_of_int (List.length cis))) in 


                            (*When backtracked to a non-bottom state, add one transition from the available set *)    
                             let (visitor, chosen) = r_choice cis a' t' s'_level gamma visitor in 
                            (* 6)   
                            (9) A” ← AddSimilarity (A’, 𝑞𝑖 ’)
                            (10) (Amin, 𝑞𝑖 min) ← Minimize (A”);
                            (11) res ← CheckSMT (Amin, 𝑞𝑖 min, Ψ)
                            (12) if (res ≠ ⊥) then return JAminK ;
                            (13) else if not max_depth then
                            (14) DFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ);
                            (15) else BFSEnumerate (Amin, 𝑞𝑖 min, L, Ψ)  *)
                
                           (match chosen with  
                           (* (match cis with  *)
                               | None -> 
                                    Message.debug ("****************NO DFS Choice, call DFS again to backtrack and 
                                                                    try a different choice**************");
                                    bfsProgress a t gamma backtracked visitor goal
                               | Some (a_new, _, t_new) -> (*Keep going*)
                                    Message.debug ("****************NEXT CHOICE Found Check if Goal is Recahed ************** ");
                                    
                                    (match t_new with 
                                        | Term s_new -> 
                                            let (a_min, t_min) =
                                            (if (!minimization) then 
                                                let updated_hedges = PTA.add_hyper_edges a_new s_new in 
                                                let PTA.PTA {q;f;qf;delta;edges} = a_new in 
                                                let a_new = 
                                                PTA.PTA {
                                                 q= q; 
                                                 f=f; 
                                                 qf=qf; 
                                                 delta = delta; 
                                                 edges = updated_hedges} in 
                                                 let _ = Message.debug ("Minimizing") in 
                                                PTA.minimize a_new t_new  
                                                
                                            else
                                                (a_new, t_new)
                                            )         
                                             in 
                                            Message.debug ("Minimizing Done ");
                                            dfsProgress a_min t_min gamma backtracked visitor goal

                                        | Bottom -> raise (PTASynthesis "Chosen Frontier in BFS else branch cannot be a BOTTOM")
                                    )
                            )
                        )
                    )    
            )                    
(*
initialize_test
goal : (z : {v : [a] | true}) -> { v : [a] | true};
output : \bot --> z -> . |- z : {v : [a] | true}
*)
let initialize (libraries : Gamma.t) 
                (gamma : VC.vctybinds) 
                (goal : RefinementType.t) : (PTA.t * PTA.frontier) = 
    Message.debug ("Initializing the PTA");
    Message.debug ("Original Query Goal "^(RefTy.toString goal));
    let bot = PTA.bottom_state () in
    Message.debug ("Adding the uniuqe bottom state in Q ");    
    let initQ_bot = [bot] in
    match goal with 
        (*Depricated now, we need to add all the libraries to the initQ even 
                               in this case  *)
        | RefinementType.Base (v, t, phi) -> 
                let bottomPTA = PTA.bottomPTA in 
                (bottomPTA, PTA.Bottom)  
            
        | RefinementType.Arrow ((_, _), _) -> 
                (*We also need to add the inital transitions like x -> phi to the FTMAP. Bottom -> (x -> phi)*)
                let uncurried = RefinementType.uncurry_Arrow goal in 
                let RefinementType.Uncurried (args_list, retType) = uncurried in 
                 
                 (* for each query argument we create
                 a node/state :
                   e.g.  x : {a | phi_x} -> y : { b | phi_y} -> {...} 
                   we get (Gamma.empty, x, {a |...}, 0 ), (x : { a | ...} |-, y ,..,0) *)
                Message.debug ("Adding formal arguments to Initail Set of States Q");
                let (initGamma, initQ_args) = List.fold_left (fun (acc_gamma,acc_qlist) (vi, ti) -> 
                                        
                                        let qi = 
                                            match ti with 
                                                | RefTy.Star -> raise (Unimplemented)
                                                | _ ->    (acc_gamma, (PTA.Exp vi), (PTA.First ti), 1) in 
                                        let acc_gamma = ((vi, ti)::gamma) in
                                        let acc_qlist = (qi:: acc_qlist) in  
                                        (acc_gamma, acc_qlist))  ([],[]) args_list in 
                                       
                                        
                (* Handling HOF,Add the type for each function in the library as an argument as well
                  HOF support is limited they are either an argument to a function calls(hence addition
                  of functions to init set) or a return of partial applications
                  The partial applications further increase the search space.

                  TODO :: Add the search for scalar or arrow in the application,

                  The partial application


                  *)      
                
                
                
                
                Message.debug ("Adding All the functions as State in Q");  
                let initQ_libraries = List.fold_left (fun acc_qlist (vi, ti) -> 
                                        match ti with 
                                            | RefTy.Arrow ((_,_),_) -> 
                                                    (*Scalar nodes for library functions, so empty environment*)
                                                    let qi = (Gamma.empty, (PTA.Exp vi), (PTA.First ti), 2) in 
                                                    (qi :: acc_qlist)
                                            | _ -> acc_qlist    
                                            ) [] libraries    
                                                
                                                
                in 


                (* let (initQ, init_delta) = List.split initQ_initDelta in    *)
                let qf_var = Var.let_binding_var () in                       
                let initQf = (initGamma, (PTA.Exp qf_var), (PTA.First retType), 100) in 
                let init_alphabet = List.map (fun (vi, ti) -> 
                                                 let arity_ti = RefinementType.arity ti in 
                                                 (vi , arity_ti)
                                                 ) gamma in 

               



                
                (* Initial delta must have all the leave transitions to each of the arguments the query
                Initial frontier should be 'b' states from these.
                In case of HOF, we shouls also have each function in the library as a part of the transition.*)
                (* For each state q = (Gamma, sv, st, level) \in Q, add a 
                   transition ((sv.string, level), [\Bottom], q) *)
                Message.debug ("Adding initial transition from bot -> ");      
                let initial_delta = List.map (fun qi -> 
                        let (gi, svi, sti, li ) = qi in 
                        let refti = match sti with 
                            | PTA.First rti -> rti
                            | PTA.Higher _  -> raise (PTASynthesis "Unimplemented")
                        in
                        let symbol_i = (PTA.State.getStateVar qi, RefTy.arity refti) in 
                        let sources_i = initQ_bot in 
                         (symbol_i, sources_i, qi)
                        ) (initQ_args@initQ_libraries) in

                let initQ = initQ_bot@initQ_args@initQ_libraries in 
                (* randomly chose b non-bottom elements in Q as the frontier
                b = 1 currently *)
                let frontier =   
                    if (List.length initQ <= 1) then 
                            PTA.Bottom 
                    else 
                        PTA.Term(List.hd (Pta.rand_select initQ_args 1))
                in        
                (PTA {q=initQ;
                f = init_alphabet;
                qf = [initQf];
                delta= initial_delta;
                edges = []}, 
                frontier)


        | _ -> raise (PTASynthesis "Unhandled Goal Type")        
(*Gamma just has the arguments provided to the query*)
(* In this implementation the beam width b = 1, we come to modified beam width later  *)

let ptaEnumerate (gamma : Gamma.t) 
                (* gamma has all the libraries *)
                 (quals :  SpecLang.Qualifier.t list) 
                 (query : RefinementType.t) 
                 (min : bool)
                 (lca_param : bool)
                 (maxPathLength : int) : 
                                (PTA.t * string * Syn.monExp list) = 
        let _ = max_depth := maxPathLength in   
        minimization := min;
        lca := lca_param;     
        qualifiers := quals;   
        Pta.qualifiers := quals;
        PTA.lca_saving_list := [];
        let (gamma, goal) =
            match query with 
             | RefTy.Arrow (rta, rtb) -> 
               Message.debug "Show ***************************************************************************";
               Message.debug "Show ***********Query: An Arrow Type********************************************";
               Message.debug "Show ***************************************************************************";
               let uncurried_spec = RefTy.uncurry_Arrow query in 
               let RefTy.Uncurried (fargs_type_list, retT) = uncurried_spec  in
               (*extend gamma*)
               (* Add the argument lists to the gamma *)
               let gamma = Gamma.append  gamma fargs_type_list in 
               (gamma, retT)     
             | RefTy.Base (v, t, pred) -> 
                (*No arguments to add to the gamma *)
               Message.show "Show ***************************************************************************";
               Message.show "Show ***********Query: A Base Type**********************************************";
               Message.show "Show ***************************************************************************";
               (gamma, query)
        in           
        Message.show ("******INITIALIZING*************");
        let libraries = gamma in 
        let initial_env = Gamma.empty in
        (* initialize and  chose a frontier 
           A0 ← initilize (L, Ψ)
        (2) 𝑞0𝑖 ← R_Choice (A0, b)
        *)
        let (initA, initT) = initialize libraries initial_env (query) in 
        Message.debug ("******INITIALIZING COMPLETE Calling DFSPROGRESS*************");
        Message.debug (PTA.string_for_pta initA);
        Message.debug ("Frontier "^(PTA.string_for_frontier initT));
        let var_forInitialTransition = 
            match initT with 
                | Bottom -> raise (PTASynthesis "Bottom cant be the initial Frontier")
                | Term s -> 
                    PTA.State.getStateVar s 
        in
        
        (* initialize the FTMap *)

        (* let initV = FTMAP.add (FTMAP.empty) (PTA.Bottom)  in  *)
        (* let initialTheta = PPTerm.ppredicate.init () in  *)
        Message.debug (" GAMMA BEFORE DFS PROGRESS "^(VC.string_gamma gamma));
        Message.debug (" Libraries BEFORE DFS PROGRESS "^(VC.string_gamma initial_env));
        (* (3) DFSEnumerate (A0, 𝑞0𝑖 , L, Ψ *)
        (* Update the frontierMap for the bottom element *)
       
        let visitor = FTMAP.add FTMAP.empty (PTA.Bottom) ([var_forInitialTransition]) in 
        let (a_final, visitor, sols) = dfsProgress initA initT gamma FBSMAP.empty visitor goal in 
        Message.show ("******DFSPROGRESS COMPLETE*************");

        Message.debug ("Final Automata,  with hyperedges "^(PTA.string_for_pta a_final));
        (* BUG! The denotation node function is incorrect *)
        (* assert (List.length sols > 0); *)
        let out = List.fold_left 
        (fun acc mi -> 
  
           
           (* let tmi = tmi.expMon in  *)
           (* Message.show ("***********\n "^(Syn.rewrite bindingExp)^"\n"^(Syn.rewrite tmi))) sols in  *)
           acc^"\n (* Program *) \n"^(Syn.monExp_toString mi)) "" sols in 
    (a_final, out, sols)     
 