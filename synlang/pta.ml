(* Define the proof tree automata and the operations over it
   This is now the term in our language.*)

(* open  Core *)
(* open Core.Std *)
open SpecLang
module Set = Stdlib.Set
exception PTAException of string     
exception Unimpl 
exception Unimplemented of string

module RefTy = SpecLang.RefinementType
module Syn = Lambdasyn
module Synthesis = Synthesis
module VC = VerificationC
module VCE = Vcencode
module Gamma = Environment.TypingEnv 
module Sigma = Environment.Constructors
module SynTC = Syntypechecker
module OEq = Orderedequivalence

let qualifiers =  ref []




let rand_select list n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n-1) (picked :: acc) rest (len-1)
  in
  let len = List.length list in
  aux (min n len) [] list len


let rec n_cartesian_product = function
  | [] -> [[]]
  | x :: xs ->
    let rest = n_cartesian_product xs in
    List.concat (List.map (fun i -> List.map (fun rs -> i :: rs) rest) x) 

module Message = struct 

  let show (s:string) = Printf.printf "%s" ("\n ")
  let debug (s:string) = Printf.printf "%s" ("\n "^s)
  (*define other utilities*)

end



module PTAutomata = struct 
  (* A function f (n1,...nn) *)
  type symbol = (Var.t * int) (*a pair of symbol and arity*)
  type alphabet = symbol list

  (* Gamma |- v : \tau @ i *)
  (* type state = Bot 
              | Internal of (Gamma.t * Var.t * RefTy.t * int) A state in our PTA is a Refinement Type at a level i *)

  type state_var = Exp of Var.t 
                 | Type of TyD.t 
  type state_type = First of RefTy.t 
                  | Higher of Kind.t


  type level = L of int | T 



  let equal_state_var sv1 sv2 = 
    match (sv1, sv2) with 
    |  (Exp v1, Exp v2) -> Var.equal v1 v2 
    |  (Type t1, Type t2) -> TyD.sametype t1 t2
    |  (_, _) -> false



  (*A state in our PTA is a Refinement Type at a level i *)
  (*Updated state for handling polymorphism; 
                    two kinds \Gamma |- v : \tau  ---- Term States
                              \Gamma |- t : k ------ Type States

  *)
  type state = (Gamma.t * state_var * state_type * int) 


  type lca_saving =   {comparing : (state * state);
                      original : int;
                      incremental : int}


  let lca_saving_list = ref ([] : lca_saving list)  





  let bottom_state_var () = (Exp (Var.fromString "BottomState"))         
  let bottom_state_type () = (First (RefTy.fromTyD TyD.Ty_unknown))

  let top_state_var () = (Exp (Var.fromString "GoalState"))
  let top_state_type query_goal = 
    match query_goal with  
    | RefTy.Base (_,_,_) -> (First query_goal)
    | RefTy.Arrow ((_,_),_)   
    | RefTy.Uncurried (_,_) -> Higher Star
    | _ -> raise (PTAException "Effectful queries not supported") 



  let bottom_state () = (Gamma.empty, bottom_state_var (), bottom_state_type (), 0) 


  let is_bottom ci = 
    let (_,_,_,level) = ci in 
    (level == 0)

  (*a symbol is a function call or other type synthesis application rule 

    We also need to define how we take the gammas for the incoming state and generate the 
    gamma for the target state*)
  type transition = (symbol * (state list) * state)


  (*An edge s1 R s2 between s1 snd s2 if s1 => s2, 
      then any path reachable from s2 is also reachable 
     from s1 and thus the exploration under s2 can be ignored*)
  type hyperedge  = (state * state)


  let state_var_toString  = function  
      Exp v -> Var.toString v 
    | Type t -> TyD.toString t


  let state_type_toString = function 
      First rt -> RefTy.toString rt 
    | Higher k -> Kind.toString k



  (************************** State definition for the QTA ***************************************)
  module State  = struct
    type t = state
    (* The compare function itself 
       is not very efficient but it must be invoked,
       right now it is invoked for bookkeeping which is very inefficinet
    *)  


  
    let getStateVar t = 
      let (_,sv,_,_) = t in 
      match sv with 
      | Exp v -> v  
      | Type td -> Var.fromString (TyD.toString td)



    let getStateType t = 
      let (_,_,st,_) = t in 
      match st with 
        First r -> r 
      | Higher k -> RefTy.Star         
    type compareres = 
      | EQ | LT | GT | U


    (* We can check definitional equality by just looking at the 
       name of the two states and their level as additional check *)
    let syntactic_equivalence t1 t2 = 
      let (_,_,_,sl1) = t1 in 
      let (_,_,_,sl2) = t2 in 

      let var_t1 = getStateVar t1 in 
      let var_t2 = getStateVar t2 in 
      (Var.equal var_t1 var_t2 && (sl1 == sl2))


    (* Check a subtyping reltaion bw states
       using the encoding in VC *)
    let states_subTypes (t1 : t) (t2 : t) : bool = 
      let (g1, v1, r1, l1) = t1 in 
      let (g2, v2, r2, l2) = t2 in 

      Message.debug ("State Subtyping Check "^(state_type_toString r1)^" <: "^(state_type_toString r2));
      Message.show("Gamma1 "^(VC.string_gamma g1)); 
      Message.show("Gamma2 "^(VC.string_gamma g2)); 
      let g  = g1@g2 in 
      let empty_delta = [] in 
      let vc_fromm_subtyping = VC.fromTypeCheck g empty_delta ((getStateType t1), (getStateType t2)) in 

      let vc_Standard = VC.standardize vc_fromm_subtyping in
      let res_lessthan =  VCE.discharge vc_Standard [] !qualifiers  in 
      let (typechecks_lessthan) = 
        match res_lessthan with 
        | VCE.Success -> true 
        | VCE.Failure -> false
        | VCE.Undef ->  false
      in
      typechecks_lessthan


    let rec compareRefTypes (gamma : Gamma.t)
                            (rt1 : RefTy.t)  
                            (rt2 : RefTy.t) :  compareres =
        (match (rt1, rt2) with 
         | RefTy.Base (vi, td1, phi1), RefTy.Base (v2, td2, phi2) ->
           if (TyD.sametype td1 td2) then 
             let (vcs_left, vcs_right) = VC.compareTypes gamma rt1 rt2 in 
             Message.show ("LEFT and RIGHT VCs generated");
             let vcleft = List.hd vcs_left in 
             let vcright = List.hd vcs_right in 
             let _ = Message.show(VC.string_for_vc_t vcleft) in 
             let _ = Message.show(VC.string_for_vc_t vcright) in 

             let vc_leftStandard = VC.standardize vcleft in
             let res_lessthan =  VCE.discharge vc_leftStandard [] !qualifiers  in 
             let (typechecks_lessthan) = 
               match res_lessthan with 
               | VCE.Success -> true 
               | VCE.Failure -> false
               | VCE.Undef ->  false
               (* raise (SynthesisException 
                  "Typechecking Did not terminate")   *)
             in
             let vc_rightStandard = VC.standardize vcright in
             let res_greaterthan =  VCE.discharge vc_rightStandard [] !qualifiers  in 
             let (typechecks_greaterthan) = 
               (match res_lessthan with 
                | VCE.Success -> true 
                | VCE.Failure -> false
                | VCE.Undef ->  false)
               (* raise (SynthesisException 
                  "Typechecking Did not terminate")   *)
             in
             (* Case when phi1 => phi2 and phi2 => phi*)
             (* Result.Res (Some appType, Model.Unsat) *)

             (if (typechecks_lessthan && typechecks_greaterthan) then EQ
              else if (typechecks_lessthan) then  LT
              else if (typechecks_greaterthan) then GT
              else U
             )
           else (*base types are not same thus incomparable*)
             U 
         | (RefTy.Arrow ((va1,ta1), tr1), RefTy.Arrow ((va2,ta2), tr2)) -> 
           let _ = Message.show ("**************") in 
           (* comparteTypes internally calls  *)
           let (vcs_left, vcs_right) = VC.compareTypes gamma rt1 rt2 in 
           Message.show ("Arrow Case LEFT and RIGHT VCs generated");


           let vcs_left_standard = List.map (fun vcleft -> VC.standardize vcleft) vcs_left in
           let vcs_right_standard = List.map (fun vcright -> VC.standardize vcright) vcs_right in

           let left_results = VCE.discharge_VCS vcs_left_standard [] !qualifiers in 
           let right_results = VCE.discharge_VCS vcs_right_standard [] !qualifiers in 
           let typecheck_left_list_len = List.length (List.filter (fun bi -> bi == false) left_results) in 
           let typecheck_right_list_len = List.length (List.filter (fun bi -> bi == false) right_results) in 

           if (typecheck_left_list_len == 0 && typecheck_right_list_len == 0) then 
             EQ
           else if (typecheck_left_list_len == 0) then 
             LT
           else if (typecheck_right_list_len == 0) then 
             GT 
           else U
         | (RefTy.PRT {params=p1;tyvars=tv1;refty=refty1}, RefTy.PRT {params=p2;tyvars=tv2;refty=refty2}) -> 
           (* logic for comparing PRT, comparision has to be just on the reftypes
              we can also compare the State.shape of params, but we dont
           *)
           compareRefTypes gamma refty1 refty2

         | (_,_) -> raise (PTAException "the states currently are only Base Refinement Type")
      )
  

    

    (* The general similarity checking function between state
       The mode of PTA compare whithout LCA *)
    let rec compare t1 t2 : (compareres)= 
      let (g1, v1, r1, l1) = t1 in 
      let (g2, v2, r2, l2) = t2 in 
      Message.debug (" State 
                       Similarity 
                      Check "^(state_type_toString r1)^" ~~ "^(state_type_toString r2));


      let g = g1@g2 in  
      
      (match (r1, r2) with 
       | (First rt1, First rt2) ->
         compareRefTypes g rt1 rt2
       | (Higher k1, Higher k2) -> 
          if (SpecLang.Kind.equal k1 k2) then 
            EQ 
          else U 
       | (_, _) -> U
      ) (*match end*)      
  


    (****An optimized comparison with a lca using quiotient informatio*****)
    let incremental_compare (s_lca : t list) (s1 : t)  (s2 : t) : (compareres) = 
      if (List.length s_lca == 0) then
        compare s1 s2
      else 
        let (g1, v1, r1, _) = s1 in 
        let (g2, v2, r2, _) = s2 in 
        let random_lca = List.hd (rand_select s_lca 1) in 
        let (g_lca,v_lca,r_lca,_) = random_lca in 
        Message.debug (" State 
                         Similarity 
                        Check "^(state_type_toString r1)^" ~~ "^(state_type_toString r2));


        match r_lca with 
          | Higher _ -> raise (Unimplemented "Higher Kinds in LCA needs to be handled")
          | First rt_lca -> 
              let _ = Message.debug ("Calculating the difference type wrt to the lca_type") in
              
              
              let g_lca_with_lca_var = g_lca@[((getStateVar random_lca), rt_lca )] in  
              
              let g1_minus_g = Gamma.diff g1 g_lca_with_lca_var in 
              let g2_minus_g = Gamma.diff g2 g_lca_with_lca_var in 


              let g_quotient = g_lca_with_lca_var@g1_minus_g@g2_minus_g in 

              let g_original = g_lca@g1@g2 in
              let size_original =  List.length g_original in 
              let size_quotient = List.length g_quotient in 
              assert (size_original >= size_quotient); 
              let reduction_in_conjuncts = (List.length g_original - (List.length g_quotient)) in 
              let saving = {comparing = (s1, s2); 
                            original = size_original;
                            incremental = size_quotient} in 

              lca_saving_list := saving :: (! lca_saving_list); 

              (match (r1, r2) with 
                | (First rt_s1, First rt_s2) ->
                   (* 
                   NOTE: We do not need to defined diff_type,
                   simply reducing the Gamma is sufficient
                  *)
                   compareRefTypes g_quotient rt_s1 rt_s2 
                | (Higher k1, Higher k2) -> 
                    if (SpecLang.Kind.equal k1 k2) then EQ else U 
                | (_, _) -> U
              ) (*match end*)      








    let toString qi = 
      let (gi, vi, ri, level) = qi in 
      let gi_string = Gamma.toString gi in 
      "( "^(state_var_toString vi)^"@"^(string_of_int level)^" \n : "^(state_type_toString ri)^" )"
    (* ("<state-begin> \n Gamma ="^(gi_string)^(" |- \n Variable ")^(Var.toString vi)^(", \n Type ")^(RefTy.toString ri)^(", \n LEVEL ")^(string_of_int level)^"\n <state-end>")  *)


    (* compare the State.shape of two states 
       The arity is to compare the two Arrow states.   
    *)
    let shape s1 : (int * TyD.t) =
      let (_,_,ty,_) = s1 in 
      match ty with 
      | First rt1 -> 
        let arity = RefTy.arity rt1 in 
        let base_type = RefTy.toTyD rt1 in 
        (arity, base_type)
      | Higher k -> 
        raise (PTAException "Unimplemented")      

  end (*End state module*)
  (*define ordered state equivalenece*)


  (************** Equivalnece/Similarity set for the States and related operations ********)
  module StateEquivalence = OEq.OrderededEquivalence (State)   

  type equivalence = StateEquivalence.t 

  (*UnionFind on the StateEquivalenec*)
  module StateUnionFind = struct 

    (* there are list of equivalence  *)
    type t = equivalence list 
    let union (eq1 : equivalence) (eq2 : equivalence) : equivalence  = 
      eq1@eq2  


    (*string for all equivalences change*)
    let toString (eqSet : equivalence) = 
      List.fold_right (fun el acc_str -> 
          ((State.toString el)^" <: "^(acc_str))) eqSet "" 

    (*state equivalence el is a state
      A sequential search in the equivalence classes   
    *)
    let rec find (el : StateEquivalence.el) t : (equivalence option) = 
      match t with 
      | [] -> 
        None
      | x :: xs -> 
        (*if el is in x then return that*)

        if (StateEquivalence.mem el x) then 
          (Some x)
          (* recursively find *)
        else (find el xs)



    (*incremental checking for equivalences*)


    (*use the hyper edges to create the equivalence classes
      The difference between this and a union find setting is 
      that the equivalence sets are ordered   
      Buggy: The Equivalence.mem function uses the compare function compare : el -> el -> int, however the 
      compare function requires a complete order between elements , while we have partial order 
      so two elements can be incomparable. I believe we can directly use a list of elements and define our 
      own compare function. Come back to this after the HOF.
    *)

    (*make from hypeedges*)
    let create_ordered_equivalences 
        (lselemts : hyperedge list) : (t) = 
      (* for each hedge = si, sj
          if (not exists a Cj, such that si or sj \in Cj) then 
              create an equivalence class for si = Ci = {si, sj}
          else 
              case Cj = {sj, sk....
                  Cj = {si, sj, sk....}
              case Cj = {sk, sj, ...}
                   s = compare (si, sk) 
                   s==si => Cj = {si, sk, sj, ...}
                   s==sk => Cj {sk , si, sj,... }
              case Cj = {si, sk, ....}
                  Cj = {si, sk, sj, ...}

      *)
      (* find method of union find*)    

      List.fold_left (fun eq_class_set hedgei -> 
          let (si, sj) = hedgei in 
          (match (find (si) eq_class_set, find (sj) eq_class_set) with 
           | (None, None) -> let new_Ci = StateEquivalence.singleton si  in
             let new_Ci = StateEquivalence.add sj new_Ci in 
             (new_Ci :: eq_class_set)
           | (Some ci, Some cj) -> 
             (* let rep_ci = Equivalence.min_elt ci in 
                let rep_cj = Equivalence.min_elt cj in  *)
             (* remove the two ci and cj *)
             let eq_class_set = List.filter 
                 (fun c -> if (c = ci || c = cj) then false 
                   else true) eq_class_set in 
             (union ci cj) :: eq_class_set
           | (_, _) -> raise (PTAException ("Unhandled") )    
          )         
        ) [] lselemts


    let check_equivalences (s : state) (eq_j : StateEquivalence.t ) = 
      let min_el = StateEquivalence.min_elt_opt eq_j in 
      match min_el with 
      | None -> None
      | Some el -> Some (State.compare s el) 



  end (*end State Union FIn*)
  module SUF = StateUnionFind

  module Ancestors = Set.Make (struct
      type t = state
      (* The comparison is using the level of the set *)
      let compare (a1 : t) (a2 : t) 
        =
        let (_,_,_,l1) =  a1 in 
        let (_,_,_,l2) = a2 in 
        compare l1 l2
    end)


  (**************************************** Definition QTA ************************************)
  let level (s : state) = 
    let (_,_,_,l) = s in l


  (*  We should also keep the height of the PTA *)

  (* 
  Acyclic Qualified Tree Automata
  A = (Q,F, Qf, ∆, \mathcal{E}
  *)
  type t = PTA of {q: state list;
                   f : alphabet;
                   qf : state list;
                   delta : transition list;
                   edges : hyperedge list}



  type frontier =  
      Bottom (* a state which is true*)
    | Term of state  

  let bottomPTA = PTA {q =[]; 
                       f = []; 
                       qf = []; 
                       delta = [];
                       edges = []}    







  let compare_transition d1 d2 = 
    let (s1, qs1, st1) = d1 in 
    let (s2, qs2 , st2) = d2 in 
    (*compaing the n*)
    Var.equal s1 s2 

  let getStates t = 
    let PTA {q;_} = t in 
    q 

  let getState_for_i t i = 
    let states = getStates t in 
    List.filter (fun qj -> let (_, _, rj, j) = qj in 
                  if (i == j) then true else false) states 

  (* Get all the states upto this level *)
  let rec get_lower_states t i = 
    if (i==0) then getState_for_i t 0
    else 
      (getState_for_i t i)@(get_lower_states t (i-1))    




  let getAlphabet t =        
    let PTA {f;_} = t in 
    f

  let getFinalStates t = 
    let PTA {qf;_} = t in 
    qf

  let getDelta t = 
    let PTA {delta;_} = t in 
    delta

  let getHyperEdges t = 
    let PTA {edges;_} = t in 
    edges

  let get_height t =  raise (Unimpl)



  (********************* Operations and Similarity checks on  Nodes************)  

  let rec instersection_states (a : t)  
      (s1 : state) 
      (s2 : state) : t =



    raise (Unimpl)


  let rec intersection (a1 : t)  (a2 : t) : t = 
    raise (Unimpl)

  (* Get the list of leaf states in an automata t *)
  let frontiers (a : t) : (state list) = 
    getState_for_i a (get_height a)



  let isFinal a q = 
    (* raise (PTAException "FORCED"); *)
    List.mem q (getFinalStates a)

  let get_transitions (a : t) (s : state) : transition list = 
    let deltas = getDelta a in 
    let delta_incoming = List.filter (fun di -> 
        let (fi, args, targeti) = di in 
        if (targeti = s) then 
          true 
        else false) deltas in 
    delta_incoming                            

  let parents (a : t) (s : state) : (state list) = 
    let incoming_transitions = get_transitions a s in 
    let parents_di_list = List.map (fun d_incoming_i -> 
        let (_, srcs_i,_) = d_incoming_i in 
        srcs_i
      ) incoming_transitions in 
    List.concat parents_di_list


  let is_parent (a : t) (parent : state) (current : state) = 
    List.exists (fun si -> 
        State.syntactic_equivalence si parent
      ) (parents a current)


  let ancestors_set (a : t) (current : state) : Ancestors.t = 
    (* for all parents, find ancestors
       if bottom-return *)
    let rec ancestors_list a ci : state list = 
      if (is_bottom ci) then 
        []
      else
        let parents_ci = parents a ci in 
        let ancestors_for_parents = List.map (fun pi -> 
            ancestors_list a pi) parents_ci in 
        (parents_ci@(List.concat ancestors_for_parents))                               


    in 
    Ancestors.of_list (ancestors_list a current)



  (* The LCA algorithm, uses QTA intersection
      find_lca t2 t2 a 
      while (true)
        case t1 = t2 then t1 
        case t2 \in parent (t1)
            then t2
        case : do I define a recursive definition here, it must take into account the 
        cycles.     
  *)
  let find_lca (a : t) (s1 : state) (s2 : state) :  (state list) = 
    if (State.syntactic_equivalence s1 s2) then 
      [s1;s2]
    else 
      (* if s1 is an ancestor to s2 then s1 *)
      (if (is_parent a s1 s2) then 
         [s1]
       else if (is_parent a s2 s1) then 
         [s2]
       else
         let set_ancestors_s1 = ancestors_set a s1 in 
         let set_ancestors_s2 = ancestors_set a s2 in 

         let ancestors_intersection = Ancestors.inter set_ancestors_s1 set_ancestors_s2 in  
         (*gives a sorted list of elements of the set using the compare function *)
         let sorted_ancestors_list =  Ancestors.elements ancestors_intersection  in 

         let top_level = 
           let (_,_,_,lhd) = List.hd (List.rev (sorted_ancestors_list)) 
           in lhd 
         in 

         (* get all the elements with level equal to the top_level *)
         List.filter (fun el -> 
             let (_,_,_,lel) = el in 
             lel == top_level) sorted_ancestors_list
      )




  (* Similarity check 
  *)
  let similarity (a : t) (s1 : state) (s2 : state)  (s_q : state) : State.compareres  = 
    raise (Unimpl)


  let get_frontier_level f = 
    match f with 
    | Bottom -> 0
    | Term s -> level (s)

  (*************************Printing utilities****************************)    

  let string_for_list (ls : string list) : string = 
    ("[ "^(List.fold_left (fun acc_str stri -> acc_str^",\n "^stri) " " ls)^" ]")

  let string_for_symbol s = 
    let (me, i) = s in 
    ("< "^(Var.toString me)^" , "^(string_of_int i))    

  (* let string_for_state qi = 
     let (gi, vi, ri, level) = qi in 
     let gi_string = Gamma.toString gi in 
     "( "^(Var.toString vi)^"@"^(string_of_int level)^" \n : "^(RefTy.toString ri)^" )"
     ("<state-begin> \n Gamma ="^(gi_string)^(" |- \n Variable ")^(Var.toString vi)^(", \n Type ")^(RefTy.toString ri)^(", \n LEVEL ")^(string_of_int level)^"\n <state-end>")  *)
  let string_for_state = State.toString     
  let string_for_frontier f = 
    match f with 
    | Bottom -> "Bottom"
    | Term s -> State.toString s

  let string_for_alphabet f = "Sigma" 


  let string_for_delta d = 
    let (symbol, nis, target) = d  in 
    let string_for_arguments = List.map (fun ni -> string_for_state ni) nis in 

    ((string_for_state target)^" <--- "^(string_for_symbol symbol)^(" ( ")^(string_for_list (string_for_arguments))^" ) ")



  let string_for_edge e =
    let (source, target) = e in  
    (string_for_state source)^" ~~~~~> "^(string_for_state target)      


  let string_for_pta t = 

    let PTA {q; f; qf; delta; edges} = t in 
    let s_q = List.map (fun qi -> string_for_state qi) q in 
    let s_f = string_for_alphabet f in 
    let s_qf = List.map (fun qi -> string_for_state qi) qf in
    let s_delta = List.map (fun di -> string_for_delta di) delta in 
    let s_edges = List.map (fun ei -> string_for_edge ei) edges in 
    ("A := { \n Q := "^(string_for_list(s_q)))^(" \n f := "^s_f)^((" \n f := "^s_f))^(" \n qf := "^(string_for_list s_qf))^("\n delta := "^(string_for_list s_delta))^(" \n edges := "^(string_for_list s_edges))^"\n }"  




  (*Intersection and union of PTAs*)


  (* Merging and Reduction on the PTAs, defined over the PTAs
     (*merge two frontiers in a TA 
     val mergeStates PAT.t f1 f2 = 
     recursively merge the equivalent states
   *)
     let mergeEquivalentStates t (f1:frontier) (f2:frontier) : t = 
      (* how to merge  *)
      match (f1, f2) with    
          | (Bottom, _) -> remove_state t f1 f2(*if either of the frontier is bottom we simply remove the other node*)
          | (_, Bottom) -> remove_state t f2 f1
  *)


  (*create equivalent classes at a level using the hyper-edges*)
  let rec transitive_horizontal t_i i : (StateEquivalence.t list) = 
    let hedges = getHyperEdges t_i in 
    let states_at_i = getState_for_i t_i i in 
    let sources_at_i = List.map (fun hedgei -> let (si, ti) = hedgei in 
                                  si) hedges in 
    let targets_at_i = List.map (fun hedgei -> let (si, ti) = hedgei in 
                                  ti) hedges in 


    (*create equivalence classes from the edge*)   
    let heddges_i = List.filter (fun hei -> let (si, ti) = hei in 
                                  if not (level (si) == level (ti)) then false 
                                  else (level (si) == i)) hedges in                          
    let equivalences_at_i = SUF.create_ordered_equivalences heddges_i in 
    (*when given s1 R s2 and s2 R s3, add s1 R s3 
      the relation or the equivalence clases have an ordering
      and remove the earlier, two relaton*)
    (* let t_i = reduce t_i equivalences_at_i in  *)
    Message.debug ("Horizontal Equivalence Sets "^(string_for_list (List.map (fun eiSet -> 
        StateEquivalence.toString eiSet) equivalences_at_i)));                            

    (equivalences_at_i)

  let rec transitive_vertical t i :  (StateEquivalence.t list)= 

    if (i == 0) then 
      []
      (* let hedges = getHyperEdges t in 
         ordered_equivalence_classes hedges
      *)
    else
      let trans_vertical_at_i_minus_one = transitive_vertical t (i-1) in 

      let states_at_i = getState_for_i t i in 
      (*gives all the equivalent classes with some set at level i*)
      let rec loop states updated_equivalences = 
        match states with 
        | [] -> updated_equivalences
        | s :: s_xs -> 
          let equivalent_s = 
            List.fold_left (
              (* check all the equivalences between states at i and equilavence c
                 classes at upto i-1 *)
              fun acc ei -> ( 

                  let is_equivalent = SUF.check_equivalences s ei in 
                  match is_equivalent with 
                  | None -> acc 
                  | Some _ -> 
                    (* if (i) then add this to the equivalent set ei *)
                    let eq_s_singleton = StateEquivalence.singleton s in 
                    let extended_eqi_with_s = SUF.union eq_s_singleton ei in 
                    extended_eqi_with_s :: acc)) 
              [] trans_vertical_at_i_minus_one in 
          loop s_xs (equivalent_s @ updated_equivalences) 


      in
      let equivalences_vertical_at_i = loop states_at_i [] in 
      (* let t = reduce t equivalences_vertical_at_i in  *)
      (* //TODO: Do we need to keep track of all the earlier equivalence as well at level i-1 *)
      let res = (equivalences_vertical_at_i) @ trans_vertical_at_i_minus_one in 
      Message.debug ("Verticle Equivalence Sets "^(string_for_list (List.map (fun eiSet -> 
          StateEquivalence.toString eiSet) res)));
      res



  (*A recursive reduction algorithm similar to the 
     minimization algroithm*)
  let rec reducePTA t (equivalences: StateEquivalence.t list)  : (t) = 
    if (List.length equivalences == 0) then 
      let _ = Message.debug ("No Equivalences so skipping the reductions ") in 
      t
    else       

      let PTA {q; f; qf; delta; edges} = t in 
      (* let q_min_rep = List.map (fun si -> 
                      let rep_si = find si equivalences in 
                      match rep_si with 
                          | Some _ -> rep_si 
                          | None -> some (Equivalence.singleton si)
                      rep_si) q in  *)

      let _ = List.iter (fun qi -> Message.debug (string_for_state qi)) q in
      let q_min = List.map (fun si -> 
          Message.debug (" VISITING "^(string_for_state si));
          let rep_si = SUF.find si equivalences in 
          let _  = Message.debug ("TEST RETURN") in 
          (match rep_si with 
           | Some rep -> 
             let _ = Message.debug (" FOUND Ci "^(StateEquivalence.toString rep)) in 
             (match (StateEquivalence.min_elt_opt rep) with 
              | Some min -> min 
              | None -> raise (PTAException "Min element should not be None")
             )    
           | None -> 
             Message.debug (" FOUND Ci  NONE ");
             si
          )
        ) q in       

      Message.debug (" Q MIN ");            
      let _ = List.iter (fun qi -> Message.debug (string_for_state qi)) q_min in             
      (* let q_min_rep_filtered = List.filter (fun rep_si -> 
                                          match rep_si with 
                                              | None -> false
                                              | Some _ -> true) q_min_rep in 



         let q_min = List.map (fun ci_rep -> 
                              let Some rep = ci_rep in    
                              Equivalence.min_elt rep) q_min_rep_filtered in                                        *)
      (* 
            let qf_min_rep  = List.map (fun si -> 
                let rep_si = find si equivalences in 
                rep_si) qf in         
            let qf_min_rep_filtered = List.filter (fun rep_si -> 
                                                match rep_si with 
                                                    | None -> false
                                                    | Some _ -> true) qf_min_rep in         

            let qf_min = List.map (fun ci_rep -> 
                                        let Some rep = ci_rep in 
                                        Equivalence.min_elt rep) qf_min_rep_filtered in  
                                         *)
      let qf_min = List.map (fun si -> 
          let rep_si = SUF.find si equivalences in 
          (match rep_si with 
           | Some rep -> 
             (match (StateEquivalence.min_elt_opt rep) with 
              | Some min -> min 
              | None -> raise (PTAException "Min element should not be None")
             )
           | None -> si)
        ) qf in                              

      (* abstraction level is state nothing changes with changed definition of the state *)
      let delta_min = List.map (fun d -> 
          let (symboli, children, target) = d in 
          let children_rep' = List.map (fun ci -> 
              SUF.find ci equivalences) children in 
          let children_rep_filtered' = List.filter (fun ci -> match ci with 
              | None -> false 
              | Some _ -> true) children_rep' in 

          let children' = List.map (fun ci_rep' -> 
              let Some rep = ci_rep' in 
              StateEquivalence.min_elt rep) children_rep_filtered' in 

          let target_rep' = SUF.find target equivalences in                                 
          let target' = 
            (match target_rep' with 
             | None -> target
             | Some c -> StateEquivalence.min_elt c) in 

          (symboli, children', target')
        ) delta in 
      let edges' = [] in 
      PTA {q=q_min;
           f = f;
           qf = qf_min;
           delta=delta_min;
           edges = edges'}     






  (*  A minimization algorithm, 
      the equivalence information is present in the
      hyper edges, returns a new A and frontier
  *)
  let minimize (original : t) (original_frontier : frontier) : (t * frontier) = 
    (* lowest level to check similarity *)
    let init_level = 0 in 
    (* let h = height (t) in *)
    let h = 4 (*fixed for testing as of now*) in 

    (* EQ ← R_Equivalences (A) *)
    let states = getStates original in 
    (*a loop to accumulate equivalent sets*)
    let rec accumulate_equivalences t_i equivalences i : (equivalence list) = 
      if (i == h) then equivalences
      else if (i <= h) then
        let (equi1) = transitive_horizontal t_i i in
        let (equi2) = transitive_vertical t_i i in 
        let equi_i =  equi1@equi2 in 
        accumulate_equivalences t_i (equi_i@equivalences) (i+1)

      else raise (PTAException "minimization at a height greater than the original tree")
    in 

    let (equivalences)  = accumulate_equivalences original [] 2 in 
    (* TODO :: Unimplemented, how to update the frontier *)

    (* Construct the new PTA *)
    (reducePTA original equivalences, original_frontier) 


  (* Synthesis of terms by enumerating the states below the level of the frontier
     to synthesize a state for a given goal*)
  let synthesizeScalar (gamma : Gamma.t) (a : t) (f : frontier) goal : (state option) = 
    let goal = (gamma, 
                (Exp (Var.fromString "temp")), 
                (First goal), 
                0) in 

    Message.debug (" esynthesizerScalar at Frontier "^(string_for_frontier f)^" \n for Goal "^(string_for_state goal));

    (*look for scalars at level lessthan or equal to the current level of the forntier
    *)
    match f with 
    | Bottom -> List.find_opt (fun si -> 
        let compare_si_goal = State.compare si goal in 
        if (compare_si_goal = EQ || compare_si_goal = LT) 
        then true else false) (getState_for_i a 0)  
    | Term s ->  
      let (_,_,_,s_level) = s in 
      (* If the frontier is correct for the goal return then same
         This in summary makes one of the possible argument as the current frontier *)
      (* Case when the current frontier is an argument*)
      (* -1 output of the compare gives the subtyping relation *)
      let (arity_goal, shape_goal) = State.shape goal in 
      let (arity_s, shape_s) = State.shape s in 
      if ((arity_goal == arity_s) && 
          (TyD.same_shape shape_goal shape_s)) then 
        let _ = Message.debug ("Shape(Frontier) == Shape(Goal), Now checking the subtying relation") in        

        if (State.states_subTypes s goal) then 
          let _ = Message.show ("Current frontier is an argument") in
          Some s
        else (*look into other states *)
          let _ = Message.debug ("Current frontier is not an an argument Finding elsewhere") in
          List.find_opt (fun si -> 
              let (arity_si, shape_si) = State.shape si in 
              if (TyD.same_shape shape_goal shape_si &&
                  (arity_si == arity_goal) && 
                  (isFinal a si = false)) then 
                (State.states_subTypes si goal) 
              else
                false   
            ) (get_lower_states a s_level) 
      else 
        let _ = Message.debug ("Shape (Frontier) != Shape (Goal) ") in        
        let _ = Message.debug ("Current frontier is not an an argument Finding elsewhere") in
        let choice = List.find_opt (fun si -> 
            let (arity_si, shape_si) = State.shape si in 
            if (TyD.same_shape shape_goal shape_si &&
                (arity_si == arity_goal) && 
                (isFinal a si = false)) then 
              let _ = Message.debug ("Doing subtyping check") in 
              (State.states_subTypes si goal) 
            else
              false   
          ) (get_lower_states a s_level) in 
        Message.debug ("Synthesized a state for the goal "^(string_for_state goal));
        choice



  (* 
    check if given the given automata can be added with hyper edges 
    when we added a state new_state 
    currenlty: b = 1    
    *)
  let add_hyper_edges (a : t) (new_state : state) (lca : bool) : (hyperedge list) =  

    let _ = Message.debug ("Adding Hyper edges for "^(string_for_state new_state)) in 
    let (_,_,_,ns_l) = new_state in
    Message.debug (" @ LEVEL "^(string_of_int ns_l));
    let var_for_ns = State.getStateVar new_state in 

    let (arity_ns, tyd_ns) = State.shape new_state in     
    (* Just collect the current hyperedges  *)
    let t_hedges = getHyperEdges a in 
    (*a single list of hedges is maintained for now
    g1 |- {v : t | \phi1} ---> g2 |- {v : t | \phi2} => g1@g2 |- \phi1 => \phi2*)



    let similarity_sets = SUF.create_ordered_equivalences t_hedges in 

    (*
     Function to add all the hyper-edges at a level 
     Optimized checking of similarity which 
     compares only against the rep eleemnt of the similarity set of a state
     rather than between each element
    *)
    let rec check_hedges_at_level newstate level hedges : (hyperedge list) = 
      let states_at_level = getState_for_i a level in 
      (* also initialze a Map [rep_li -> comparison result] for the current new_state *)
      let rep_result_pairs = ([] : (state*State.compareres) list) in

      (* Tbe iteration over all the sttes found at the evel *)
      let rec loop_over_states_at_level state_list found_hedges (recorded_results : (state*State.compareres) list) : (hyperedge list) = 
        match state_list with 
        | [] -> found_hedges
        | s_li :: s_ls ->
          (*Check if we are looking at the same state*)
          if (State.syntactic_equivalence s_li new_state) then 
            let _ = Message.debug ("Comparing Against the Node "^(Var.toString var_for_sli)) in 
            loop_over_states_at_level s_ls found_hedges recorded_results
          else
            let eq_sli_opt =  SUF.find s_li similarity_sets  in
            let rep_sli = 
              match eq_sli_opt with 
              | None -> 
                s_li (*No equivalenece set means reflexive eq*)
              | Some eq_sli -> StateEquivalence.min_elt eq_sli 
            in   
            (* see if we already have the comparison result *)
            let recorded_res = List.assoc_opt rep_sli recorded_results in    
            (match recorded_res with 
             | Some cres -> 
               (* We do not need to call the compare in this case *)
               (match (cres) with 
                | U -> loop_over_states_at_level s_ls found_hedges recorded_results
                | LT  -> 
                  (* rep <: new_state, we can add an edge (rep, new_state) *)
                  loop_over_states_at_level s_ls ((rep_sli, new_state):: found_hedges) recorded_results
                | GT -> 
                  (* rep > new_state, we add (new_state, rep_sli) *)
                  loop_over_states_at_level s_ls ((new_state,rep_sli)::found_hedges) recorded_results
                | EQ -> 
                  (* The hedges has both *)
                  loop_over_states_at_level s_ls ((new_state, rep_sli)::((rep_sli, new_state):: found_hedges)) recorded_results
                  (*In case t1 <: t2 and t2 <: t1, we are just keeping t1 <: t2
                     this does not cause us to loose precision as t2 is removed anyways*)
               )                                 

             | None ->      
                (*No pre-recorded results, two possible cases based on the comparison choice*)
                (*Based on the mode of the synthesis, 
                either use a LCA based incremental comparison or 
                standard comparison
                *)
               if (lca) then  
                   let lca = find_lca a rep_sli new_state in 
                   let new_comp_res = State.incremental_compare lca rep_sli new_state in 
                   let recorded_results = ((rep_sli, new_comp_res) 
                                         :: recorded_results) in 

                  (match (new_comp_res) with 
                   | U -> loop_over_states_at_level s_ls found_hedges recorded_results
                   | LT  -> 
                     (* rep <: new_state, we can add an edge (rep, new_state) *)
                     loop_over_states_at_level s_ls ((rep_sli, new_state):: found_hedges) recorded_results
                   | GT -> 
                     (* rep > new_state, we add (new_state, rep_sli) *)
                     loop_over_states_at_level s_ls ((new_state,rep_sli)::found_hedges) recorded_results
                   | EQ -> 
                     (* The hedges has both *)
                     loop_over_states_at_level s_ls ((rep_sli, new_state):: found_hedges) recorded_results
                     (*In case t1 <: t2 and t2 <: t1, we are just keeping t1 <: t2
                       this does not cause us to loose precision as t2 is removed anyways*)

                  )
               else (*Non-LCA comparison*)
                   let lca = find_lca a rep_sli new_state in 
                   let new_comp_res = State.compare rep_sli new_state in 
                   let recorded_results = ((rep_sli, new_comp_res) 
                                         :: recorded_results) in 

                  (match (new_comp_res) with 
                   | U -> loop_over_states_at_level s_ls found_hedges recorded_results
                   | LT  -> 
                     (* rep <: new_state, we can add an edge (rep, new_state) *)
                     loop_over_states_at_level s_ls ((rep_sli, new_state):: found_hedges) recorded_results
                   | GT -> 
                     (* rep > new_state, we add (new_state, rep_sli) *)
                     loop_over_states_at_level s_ls ((new_state,rep_sli)::found_hedges) recorded_results
                   | EQ -> 
                     (* The hedges has both *)
                     loop_over_states_at_level s_ls ((rep_sli, new_state):: found_hedges) recorded_results
                     (*In case t1 <: t2 and t2 <: t1, we are just keeping t1 <: t2
                       this does not cause us to loose precision as t2 is removed anyways*)

                  )
            )    
      in
      loop_over_states_at_level states_at_level hedges rep_result_pairs
    in  


    (* The iteration over all the levels and calling 
       the function to find h_edges at all the levels (level....0)
    *)
    let rec hedges_accumulate level initial_hyper_edges : (hyperedge list) = 
      (* create an indexed list of lists
         [[],[],....] *)
      if level = 0 then
        let states_at_level0 = [bottom_state ()] in 
        (* Calculate the hyperedges at level 0 
           Nothing is similar to Bottom so base case *)
        initial_hyper_edges
      else 
        let states_at_level = getState_for_i  a level in 
        let res_at_level_minus_one = hedges_accumulate (level-1) initial_hyper_edges in 
        (* Calculate hyperedges at level level*)
        let new_hedges_at_level = check_hedges_at_level new_state level initial_hyper_edges in 
        res_at_level_minus_one@new_hedges_at_level
    in    

    (* finaly call the outer loop over all the levels *)
    hedges_accumulate ns_l t_hedges 



  (* routine to add a transition by 
      1. function application, rule T-APP HO 
      2. TypeVariable Substituttion rule T-alpha
      3. Parameter substitution. T-App parameer 
  *)
  let addEdgeAndTransition (a : t) 
      (front : frontier) 
      (level : int)
      (gamma : Gamma.t) 
      (quals : SpecLang.Qualifier.t list)
      (*The component name and the arity chosen based on a ranking*)
      (ci : (Var.t * int)) : (t * transition * frontier) option = 

    let PTA {q; f; qf; delta; edges} = a in
    qualifiers := quals;
    let (var, arity) = ci in 
    let ci_type = Gamma.find gamma var in 
    Message.debug ("Trying Edge for Component "^(Var.toString var)^" \n :"^(RefTy.toString ci_type));

    (*A function to synthesize_arguments for complete or partial application*)
    let rec synthesizeArguments ci_type synth_list: (state list * RefTy.t)= 
      (match ci_type with 
       (*Polymprohic and parametric type of a function or a Base*)
       | RefTy.PRT {tyvars;params;refty} -> raise Unimpl
       | RefTy.Arrow ((arg, argty), body_type) -> 
         Message.debug ("Synthesizing Arguent for the formal parameter "^(Var.toString arg)^" \n :"^(RefTy.toString argty));
         let node_for_arg = synthesizeScalar gamma a front argty in 
         (* try next argument if the current one succeeded, else return *)

         (match node_for_arg with 
          | Some x -> 
            let _ = Message.debug ("Successfully Synthesized argument for "^(Var.toString arg)^" Next getting the type after partial application") in 
            Message.debug ("Synthesized State "^(string_for_state x));
            let (_,vx, tx,_) = x in

            (match (vx, tx) with 
             (* Synthesized argument is a first-order node
             *)
               (Exp evx, First rtx) -> 
               (*TODO :: Case split on the basis of vx and tx*)
               let (partial_application_body_type) = SynTC.synthesizeType4Transition 
                                                            gamma ci_type evx rtx in 
               Message.debug (" Type for partial application Type "^(RefTy.toString partial_application_body_type));
               Message.debug ("Synthesizing Next argument");
               synthesizeArguments partial_application_body_type (synth_list @ [x]) 
             | (_, _) -> 
               raise (PTAException "Illegal Node Type, neigher |- e : \tau nor |- alpha : k")    
            )
          | None ->
            let _ = Message.debug ("Failed Synthesizing argument returning the partial application type and args") in 
            (synth_list, ci_type)
         )    
       (* Synthesize nodes for partial or complete list of arguments *)
       | _ -> 
         let _ = Message.debug ("The partially applied function is not an Arrow") in 
         (synth_list, ci_type)
      )    
    in     

    (match ci_type with 
       (* ci has a polymorphic type like for map : <p : b :-> bool>. (a -> {v : b| p b) -> [a] -> 
                                                                            {v : [b] | \u : b. u \in mem (v) => p b }*) 
     (* look for T : * and p of type  p :-> bool, such that we can also get the function
                             (a -> {v : b | p b}) *)


     (*Based on wether there is refinement parameter p or not, we apply rules 
       when p is not present. 
       Γ |- e : ∀α.σ Γ ? τ 
       Γ ? e [τ] : σ[τ/α]  T-INST

       when p is also present 
       Γ ? e : ∀π : sort.σ       Γ |-  pred : sort
       Γ ? e [pred] : σ[π |-> pred] 

       if both are present then we iteratively apply the two rules
      *)                               
    
     | PRT {tyvars;params;refty} ->
       raise Unimpl 
     | Arrow ((arg, argty), bodytype) -> 
       let arityFromtype = RefTy.arity ci_type in 
       assert (arityFromtype == arity);
       let (synthesized_args, app_type) = synthesizeArguments ci_type [] in 
       (* all of the arguments synthesized *)
       if (List.length synthesized_args == arity || List.length synthesized_args > 0) then 
         let progress = 
           (match front with 
            | Bottom -> true
            | Term s -> List.mem s synthesized_args) in 

         if (progress) then 
           (* This handles both the first order and higher order function application
              along with the  *)
           let target_state_refty = app_type in 
           (*Extend the Gamma now with corresponding type for each argument passed to the function*)
           let target_state_gamma = List.fold_left (fun g ni -> 
               let (gi, _, _, _) = ni in
               let vi = State.getStateVar ni in 
               let ri = State.getStateType ni in
               let gi' = (vi, ri)::gi in 
               g@gi') [] synthesized_args 

           in 

           let target_state = 
             (match target_state_refty with 
              | Star -> 
                raise Unimpl 
              | _ ->    
                let target_var = Var.let_binding_var () in 
                (target_state_gamma, 
                 (Exp target_var), 
                 (First target_state_refty), 
                 level+1)
             )                     
           in 

           (*a transition ci_level (es) ---> target_state_level+1*)
           let new_transition = (ci, synthesized_args, target_state) in 
           (* see if this node is equivalent to some other noder in the tree and
              add hyper edges in the tree
              Whendver we add a new node and edge in the tree we add corresponsing hyperedges.

           *)
           let updated_hedges = add_hyper_edges a target_state in 

           Some 
             (PTA {
                 q=((target_state)::q); 
                 f=f; 
                 qf=qf; 
                 delta = (new_transition :: delta); 
                 edges = updated_hedges},
              new_transition,
              Term target_state)

         else  
            (* No progress at this frontier *)
           (*Do we need to record this failed component from a 
             state in the visited set*)
           let _ = Message.debug ("NO progress so skipping this choice at this frontier") in              
           None


       else  (* no args synthesized*)    
         let _ = 
           Message.debug ("No args can be synthesized") in 
         (*Addition of the component is not feasible*)
         None



     | RefTy.Base (_,_,_) -> (*Base Types have no effect on the tree automaton other than in the initial phase*)
       match front with
       | Bottom -> 
         raise (PTAException "Addition to Bottom cases must happen in a separate routine");
       | Term _ -> 
         let _ = Message.debug ("Non Arrow Type in Non Bottom Case, Skipping the Edge Addition"^(RefTy.toString ci_type)) in 
         None

       | _ -> 
         raise (PTAException "Unhandled :: AddEdgeAndTransition for Unknow Component Type") 
    )   


  (*The main semantics function given the automata and the frontier 
    we run the pta and get a list of expressions*)
  let rec denotation_node (a : t)  (f : frontier) : Syn.monExp list = 
    match f with 
    | Bottom -> (*no result*) []
    | Term s -> 
      let _ = Message.debug (" Denotation Node Non-Bottom Case "^(string_for_state s)) in 
      Message.show ("PTA "^(string_for_pta a));
      (*get all the incoming edges to this node*)
      let delta_incoming =  get_transitions a s  in 

      let rec loop edges denotation_set = 
        match edges with 
        | [] -> denotation_set 
        | di :: ds -> 
          Message.debug (" The current transition "^(string_for_delta di));
          let denotation_di = denotation_edge a di in 
          loop ds (denotation_di :: denotation_set) 
      in
      let list_of_list = loop delta_incoming [] in 
      List.concat list_of_list                

  and denotation_edge (a : t) (d : transition) : Syn.monExp list = 
    let (symbol_d, qis, _) = d in 
    let (fname, _) = symbol_d in 
    (*get a list of list of Syn.monExp*)
    let list_list_args  =  List.map (fun qi -> 
        denotation_node a qi) (List.map (fun qi -> Term qi) qis) in 

    let _ = List.iter (fun li -> assert (List.length li > 0)) in 
    let args_list_choices = n_cartesian_product list_list_args in 
    List.map (fun args_list_choice_i -> 
        Syn.Eapp (Syn.Evar fname, args_list_choice_i)) args_list_choices 


  let walk_n_search (complete_pta : t) 
      (query : RefinementType.t) : (string * Syn.monExp list) = 

    (*look at all the frontier nodes*)

    raise (Unimpl)



end 







