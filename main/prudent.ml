module A = Pta
open SpecLang
module Sigma = Environment.Constructors 
module Gamma = Environment.TypingEnv
module P = Predicate 
module Syn = Lambdasyn
module SEL = SpecElab
module Synth = Synthesis
exception CompilerExc of string  
(* USe the OCaml Graph library https://github.com/backtracking/ocamlgraph *)
(* Top level Prudent application *)


let spec_file = ref ""
let minimization = ref true

let lca = ref true
let usage_msg = "prudent [-nosimilarity] [-nolca] -max <maximum-path-length> <spec-file1> "
let anon_fun specfile = 
    spec_file := specfile
let maxPathlength = ref 5   
let goal_number = ref 0
let static = ref false



module Printf = struct 
  let printf d s = Printf.printf d s
  let originalPrint = Printf.printf 
  let fprintf = Printf.fprintf
end  


  let speclist =
    (* , dynamic without similarity Hegel_2 *)
  [("-nosimilarity", Arg.Clear minimization, "Set the minimization flag to false");
    (*  dynamic, similarity without LCA Hegel_1 *)
   ("-nolca", Arg.Clear lca, "Set the LCA algorithm to false");
   (* Hegel_0 for the terms *)
   ("-static", Arg.Set static, "Set the Static building of QTA and synthesis");
   ("-max", Arg.Set_int maxPathlength, "Set the max path length")] 
  
let () = 


  Arg.parse speclist anon_fun usage_msg;
  
  let () = Printf.printf "%s" "\n  EXPLORED Args.parser output " in 
  let () = Printf.printf "%s" ("\n EXPLORED nosimilarity  "^(string_of_bool !minimization)) in 
  let () = Printf.printf "%s" ("\n EXPLORED LCA  "^(string_of_bool !lca)) in
  let () = Printf.printf "%s" ("\n EXPLORED STATIC  "^(string_of_bool !static)) in
  
  let () = Printf.printf "%s" ("\n EXPLORED Max path length :: "^(string_of_int (!maxPathlength))) in 
 
  (* raise (CompilerExc "Forced"); *)

  let ast = SEL.parseLSpecFile !spec_file in 
  let string_ast = RelSpec.toString ast in 
  let () = Printf.printf "%s" string_ast in 
  let (gamma, sigma, typenames, quals, goals) = SEL.elaborateEnvs ast in 
  let goal = List.nth goals !goal_number in
  let delta = P.True in 
  let () = Printf.printf "%s" "\n INITIAL GAMMA \n " in 
  let () = List.iter (fun (vi, rti) -> Printf.printf "%s" 
                      ("\n "^(Var.toString vi)^" : "^(RefTy.toString rti))) gamma in 


  let () = Printf.printf "%s" "\n INITIAL SIGMA \n " in 
  let () = List.iter (fun (vi, rti) -> Printf.printf "%s" 
                      ("\n "^(Var.toString vi)^" : "^(RefTy.toString rti))) sigma in 

  let () = Printf.printf "%s" "\n TypeNames \n " in 
  let () = List.iter (fun tni -> Printf.printf "%s" ("\n "^tni)) typenames in 
  

  let () = Printf.printf "%s" "\n Qualifiers \n " in 
  let () = List.iter (fun (qi) -> Printf.printf "%s" 
                      ("\n "^(SpecLang.Qualifier.toString qi))) quals in 

  (* let (outstring, synthterm) = Synth.Bidirectional.toplevel gamma sigma  delta typenames quals goal !learningON !bidirectional !maxPathlength !effect_filter !nestedif in    *)

  let (finalPTA, outstring, synthterms) = 
    if (!static) then 
     let _ = Printf.printf "%s" "Static building of QTA" in 
     StaticQTA.enumerate gamma quals goal !maxPathlength 
     else  
      AlgorithmW.ptaEnumerate gamma quals goal !minimization !lca !maxPathlength 
  in 
    (*run the initial environment builder*)    
  match synthterms with 
        | [] -> 
            let _ = Printf.originalPrint "%s" ("\n *************************") in 
            let _ = Printf.originalPrint "%s" ("\n Failed without Result : ") in 
            Printf.originalPrint "%s" ("\n ************************* : ") 
        | _ :: _ -> 
            let outchannel = open_out ("output/"^(!spec_file)) in
            Printf.fprintf outchannel "%s\n" ("(*Hegel Generated*) \n"^(outstring));
            (* write something *)
            close_out outchannel;
            let _ = Printf.originalPrint "%s" ("\n *************************") in 
            let _ = Printf.originalPrint "%s" ("\n Success : ") in 
            let _ = Printf.originalPrint "%s" ("\n ************************* : ") in 
            ()
