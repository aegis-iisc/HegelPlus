module VC = VerificationC

val z3_log : string -> unit

type result = Success | Undef | Failure

val discharge : VC.standardt -> (SpecLang.Var.t list) -> 
                    (SpecLang.Qualifier.t list) 
                    ->  result

val discharge_VCS : (VC.standardt list) ->  
                  (SpecLang.Var.t list) -> 
                  (SpecLang.Qualifier.t list) -> 
                  (bool list)  