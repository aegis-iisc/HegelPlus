type ziplist;
qualifier zdom : heap :-> ref ziplist :-> bool; 
qualifier zsel : heap :-> ref ziplist :-> ziplist;
qualifier zlen : ziplist :-> int; 
qualifier zllen : ziplist :-> int; 
qualifier zrlen : ziplist :-> int;
qualifier zmem : ziplist :-> a :-> bool;
qualifier allen : [a] :-> int;

Z : ziplist;
Z' : ziplist;

empty :  State {\(h : heap). true} 
			v : ref ziplist 
		{ \(h : heap), (v : ref ziplist), (h' : heap). 
				\(Z' : ziplist),(Z : ziplist).
		         zdom (h', v) = true  /\
                 zsel (h', v) = Z' /\ 
                 zlen (Z') == 0 /\
                 zllen (Z') == 0 /\ 
                 zrlen (Z) == 0
        };

fresh_el : (z : ref ziplist) -> 
            State 
			{\(h : heap). not (sel (h, num) > 4)} 
			v : { v : a | true} 
			{\(h : heap), (v : a), (h' : heap). 
				\(Z' : ziplist), (Z : ziplist).
				zsel (h, z) = Z /\
                zsel (h', z) = Z' /\
                sel (h', num) == sel (h, num) + 1 /\
				[Z' = Z] /\
                zmem (Z', v) = false};

is_empty : (z : ref ziplist) ->
                        State {\(h : heap). true} 
		            	    v : {v : bool | true} 
		                { \(h : heap), (v : bool), (h' : heap). 
				          \ (Z : ziplist).
		                    zsel (h, z) = Z /\
                            ([v= true] <=> zlen (Z) == 0 /\ zllen (Z) == 0 /\ zrlen (Z) == 0) /\
                            ([v= false] <=> zlen (Z) > 0 /\ 
                            ( zllen (Z) > 0 \/ zrlen (Z) > 0 )) /\
                            [h'= h]
                            };


is_left_empty : (z : ref ziplist) ->
                        State {\(h : heap). true} 
		            	    v : {v : bool | true} 
		                { \(h : heap), (v : bool), (h' : heap). 
				          \ (Z : ziplist).
		                    zsel (h, z) = Z /\
                            ([v= true] <=> zllen (Z) == 0) /\
                            ([v= false] <=> zllen (Z) > 0) /\
                            [h'= h]
                            };


is_right_empty : (z : ref ziplist) ->
                        State {\(h : heap). true} 
		            	    v : {v : bool | true} 
		                { \(h : heap), (v : bool), (h' : heap). 
				          \ (Z : ziplist).
		                    zsel (h, z) = Z /\
                            ([v= true] <=> zrlen (Z) == 0) /\
                            ([v= false] <=> zrlen (Z) > 0 ) /\
                            [h'= h]
                            };

length : (z : ref ziplist)  ->
                        State {\(h : heap). true} 
		            	    v : {v : int | true} 
		                { \(h : heap), (v : int), (h' : heap). 
				        \(Z : ziplist).
		                    zsel (h, z) = Z /\
                            zlen (Z) = v /\
                            [h=h'] 
                        };
          


to_list : (z : ref ziplist)  ->
                        State {\(h : heap). true} 
		            	    v : [a] 
		                { \(h : heap), (v : [a]), (h' : heap). 
				        \(Z : ziplist).
		                    zsel (h, z) = Z /\
                            allen (v) == zlen (Z) /\
                            [h=h']
                        };
          
make :  (l : [a]) -> 
                State {\(h : heap). true} 
			        v : ref ziplist 
		        { \(h : heap), (v : ref ziplist), (h' : heap). 
				\(Z' : ziplist),(Z : ziplist).
		         zdom (h', v) = true  /\
                 zsel (h', v) = Z' /\ 
                 zlen (Z') = allen (l) /\
                 zlen (Z') == (zllen (Z') + zrlen (Z)) 
                };


move_left : (z1 : ref ziplist) -> 
                State {\(h : heap). \(Z : ziplist).
                         zdom (h, z1) = true /\ 
                         (zsel (h, z1) = Z => zllen (Z) > 0) 
                      } 
			        v : {v : unit | true}
		        { \(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
                          zdom (h', z1) = true /\
		                  zsel (h, z1) = Z /\
                          zsel (h', z1)  = Z' /\
                          zlen (Z') = zlen (Z) /\ 
                          zllen (Z') == zllen (Z) -- 1 /\
                          zrlen (Z') == zrlen (Z) + 1 };


insert_left : (x : a) ->
              (z1 : ref ziplist) -> 
                State {\(h : heap). zdom (h, z1) = true} 
			           v : {v : unit | true}
		        {\(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
		                  zsel (h, z1) = Z /\
                          zsel (h', z1) = Z' /\
                          zlen (Z') == zlen (Z) + 1 /\ 
                          zllen (Z') == zllen (Z) + 1 /\
                          zrlen (Z') == zrlen (Z)
                };




remove_left : (z1 : ref ziplist) -> 
                State {\(h : heap). zdom (h, z1) = true} 
			           v : {v : unit | true}
		        {\(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
		                  zsel (h, z1) = Z /\
                          zsel (h', z1) = Z' /\
                          zlen (Z') == zlen (Z) -- 1 /\ 
                          zllen (Z') == zllen (Z) -- 1 /\
                          zrlen (Z') = zrlen (Z)
                };



move_all_left : (z1 : ref ziplist) -> 
                State {\(h : heap). zdom (h, z1) = true} 
			           v : {v : unit | true}
		        {\(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
		                  zsel (h, z1) = Z /\
                          zsel (h', z1) = Z' /\
                          zlen (Z') = zlen (Z) /\ 
                          zllen (Z') == 0 /\
                          zrlen (Z') == zrlen (Z) + zllen (Z)
                };
              


move_right : (z1 : ref ziplist) -> 
                State {\(h : heap). zdom (h, z1) = true} 
			           v : {v : unit | true}
		        { \(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
                          zdom (h', z1) = true /\
		                  zsel (h, z1) = Z /\
                          zsel (h', z1)  = Z' /\
                          zlen (Z') = zlen (Z) /\ 
                          zllen (Z') == zllen (Z) + 1 /\
                          zrlen (Z') == zrlen (Z) -- 1 };



insert_right : (x : a) ->
              (z : ref ziplist) -> 
                State {\(h : heap). zdom (h, z) = true} 
			           v : {v : unit | true}
		        {\(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
		                  zsel (h, z) = Z /\
                          zsel (h', z) = Z' /\
                          zlen (Z') == zlen (Z) + 1 /\ 
                          zllen (Z') == zllen (Z) /\
                          zrlen (Z') == zrlen (Z) + 1
                };




remove_right : (z1 : ref ziplist) -> 
                State {\(h : heap). zdom (h, z1) = true} 
			           v : {v : unit | true}
		        {\(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
		                  zsel (h, z1) = Z /\
                          zsel (h', z1) = Z' /\
                          zlen (Z') == zlen (Z) -- 1 /\ 
                          zllen (Z') = zllen (Z)  /\
                          zrlen (Z') == zrlen (Z) -- 1
                };




move_all_right : (z1 : ref ziplist) -> 
                State {\(h : heap). zdom (h, z1) = true} 
			           v : {v : unit | true}
		        {\(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
		                  zsel (h, z1) = Z /\
                          zsel (h', z1) = Z' /\
                          zlen (Z') = zlen (Z) /\ 
                          zrlen (Z') == 0 /\
                          zllen (Z') == zrlen (Z) + zllen (Z)
                };
              

is_focused : (z : ref ziplist) -> 
                      State {\(h : heap). true} 
		            	    v : {v : bool | true}
		                { \(h : heap), (v : bool), (h' : heap). 
				        \(Z : ziplist).
		                    zsel (h, z) = Z /\
                            [v=true] <=> zrlen (Z) > 0 /\
                            [h' = h]
                            };

focused : (z1 : ziplist) -> {v : a | true};
goal : (x : a) -> 
        (z : ref ziplist) -> 
                State {\(h : heap). 
                        \(Z : ziplist). zdom (h, z) = true /\
                                zsel (h , z) = Z /\
                                zlen (Z) = 0} 
			           v : {v : unit | true}
		        {\(h : heap), (v : unit), (h' : heap). 
				    \(Z : ziplist), (Z' : ziplist).
		                  zsel (h, z) = Z /\
                          zsel (h', z) = Z' /\
                          zlen (Z') = 3 /\ 
                          zllen (Z') = 1 /\
                          zrlen (Z') =  2
                };
 