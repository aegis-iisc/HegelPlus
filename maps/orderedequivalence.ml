module type Elem = sig 
  type t 
  type compareres = EQ | LT  |GT | U
  val compare : t -> t -> compareres
  val toString : t -> string
end 

(*An equivalence class of elements*)
module OrderededEquivalence (E : Elem) = struct 
   exception EquivalenceClassException of string 
    module Elem  = E 
    (*An eqclass is a list of elements with the following op*)
    type el = Elem.t 
    
    type t  = el list
    (*element of the equivalence class*)
    
    
    let empty  = []
    
    let is_empty t = 
        (List.length t == 0)
 
    (*find the representative elems or the min_elt*)    
    let min_elt_opt t = 
      match t with 
        [] -> None
        | _::_ ->   
           (* sort the list and get the first element *)
           let min = List.hd (List.sort (fun t1 t2 -> 
                  let compareRes = Elem.compare t1 t2  in 
                  (match compareRes with 
                    | EQ -> 0
                    | LT -> -100
                    | GT -> 100
                    | U -> raise (EquivalenceClassException "Incomparabel elems in Eq Class")
                   ) ) t 
           ) in 
           Some min 

     let min_elt t = 
      assert (List.length t > 0);
      if (List.length t == 1) then List.hd (t) 
      else
          let x :: xs = t in 
           (* sort the list and get the first element *)
          let min = List.hd (List.sort (fun t1 t2 -> 
                    let compareRes = Elem.compare t1 t2  in 
                    (match compareRes with 
                      | EQ -> 0
                      | LT -> -100
                      | GT -> 100
                      | U -> raise (EquivalenceClassException "Incomparabel elems in Eq Class")
                     ) ) t 
             ) in 
             min
           
          
     let mem el t = 
         List.mem el t

     let add el t = el :: t


     let singleton el = [el]

     (* we need to define union, intersection etc if needed *)

      let toString t = 
        List.fold_left (fun acc_str el -> 
                        ((Elem.toString el)^" ~~ "^(acc_str))) "" t 
             

end 


