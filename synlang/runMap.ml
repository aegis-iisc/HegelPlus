module Def = Definitions
module RMapKey =
       struct
         type t = Def.Position.t
         let equal (t1,t2)  =  Def.position.equal t1 t2
       end

module RMapValue = 
       struct
         type t = Def.State.t
        end



module RMap = Applicativemap.ApplicativeMap (RMapKey) (RMapValue) 



exception InvalidPos of RMapKey.t




type t = RMap.t

let empty = RMap.empty
let mem = RMap.mem
let find t pos = 
    try (RMap.find p pos) 
  with 
  | (RMap.KeyNotFound k) -> raise (InvalidPos k)

let add = fun rmap -> fun pos state -> RMap.add rmap pos state 
let remove = TypesMap.remove
