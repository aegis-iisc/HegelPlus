(*The RBTree example takes more than 30 mins to run and find programs of size upto 4
with just a library size of 12 *)

qualifier numblack : rbtree :-> int;
qualifier nordered : rbtree :-> bool;
qualifier hdcolor : rbtree :-> bool;


dummy : {v : unit | true};


bool_gen : (d:unit) -> {v : bool | [v=true] <=>[v=true] /\
                                    [v=false] <=> [v=false]};

sizecheck : (s : int) -> 
        {v : bool | [v=true] <=> [s=0] /\ 
                    [v=false] <=> [s>0]};



int_gen : (d:unit) -> { v : int | ([v>0] \/ [v=0])}; 

subs : (n : {v : int | true}) ->  {v : int | v == n -- 1};


lt_eq_one : (s : int) -> {v : bool | [v=true] <=> not [s > 1] /\ 
                            [v=false] <=> [s>1]};



int_range : (a : int) -> (b : int) -> {v : int | not [a > v] /\ not [v > b]};


increment : (n : {v : int | true}) ->  {v : int | v == n + 1};

Rbtleaf : {v : rbtree | numblack (v) == 0 /\
                        nordered (v) = false 
                           };

RbtNode1 : (c : {v : bool | [v = false]}) -> 
          (ltree : {v : rbtree | \(sizel : int).
                        ([sizel > 0] \/ [sizel = 0]) /\
                        numblack (v) == sizel /\
                        nordered (v) = true /\
                        ([sizel = 0] => hdcolor (v) = true)    
                     })  -> 
           (r : int) -> 
           (rtree : {v : rbtree | \(sizer : int), (sizel : int).
                    (numblack (ltree) == sizel) /\
                    [sizer = sizel] /\
                    (numblack (v) == sizer) /\
                    nordered (v) = true /\
                    ([sizer = 0] => hdcolor (v) = true) 
            }) -> 
        {v : rbtree | \(u : int). 
                hdcolor (v) = false /\
                ((u == numblack (ltree) + 1) => 
                    (numblack (v) == u /\ 
                    nordered (v) = true
                    )
                 )   
        };          

RbtNode2 : (c : {v : bool | true}) -> 
          (ltree : {v : rbtree | \(sizel : int).
                        ([sizel > 0] \/ [sizel = 0]) /\
                        numblack (v) == sizel /\
                        nordered (v) = true /\
                        hdcolor (v) = false    
                     })  -> 
           (r : int) -> 
           (rtree : {v : rbtree | \(sizer : int), (sizel : int).
                    (numblack (ltree) == sizel) /\
                    [sizer = sizel] /\
                    (numblack (v) == sizer) /\
                    nordered (v) = true /\
                    hdcolor (v) = false 
            }) -> 
        {v : rbtree | \(sizel : int). 
                numblack (ltree) == sizel /\
                hdcolor (v) = true /\
                numblack (v) == sizel /\ 
                nordered (v) = true
        };

goal : (inv : {v : int | [v>0] \/ [v=0]}) -> 
       (c : bool) -> 
       (height : {v : int | 
            ([v>0] \/ [v=0]) /\ 
            ([c=true] => inv == v + v) /\
            ([c=false] => inv == v + v + 1)
            }) -> 
       {v : rbtree | \(u : int).
        numblack(v) == height /\
        nordered(v) = true /\
       ( 
        ([c=true] /\ hdcolor(v) = false) \/
        ([c=false] /\ ([height=0] => hdcolor (v) = true))
        )};     


(*The SizedBST example, takes aroun 20 mins to find the programs with size 4 and nested-if depths of 5*)
          
qualifier tlen : tree :-> int;
qualifier sortedtree : tree :-> bool;
qualifier tmem : tree :-> int :-> bool;
qualifier rng : tree :-> int;
qualifier low : int :-> bool;
qualifier high : int :-> bool;




dummy : {v : unit | true};

bool_gen : (un :unit) -> {v : bool | [v=true] <=>[v=true] /\
                                    [v=false] <=> [v=false]};


lt_eq_one : (s : int) -> {v : bool | [v=true] <=> not [s > 1] /\ 
                            [v=false] <=> [s>1]};


decrement : (n : {v : int | true}) ->  {v : int | v == n -- 1};

int_range : (a : int) -> (b : int) -> {v : int | not [a > v] /\ not [v > b]};


increment : (n : {v : int | true}) ->  {v : int | v == n + 1};


Leaf : {v : tree | \(u : int).
                    rng (v) == 0 /\
                    tmem (v, u) = false /\ 
                    sortedtree (v) = true };



Node : 
    (root : { v : int | true}) -> 
    (ltree : {v : tree | \(u : int), (range1 : int).  
                    ( (tmem (v, u) = true /\ rng (v) == range1)  => 
                        ([root > u] /\ (u > root -- range1))
                    )/\ 
                    sortedtree (v) = true}
    ) -> 
    (rtree : {v : tree | \(u : int), (range2 : int).  
                    ( (tmem (v, u) = true /\ rng (v) == range2)  => 
                       ([u > root] /\ (root > u -- range2))
                    )/\ 
                    sortedtree (v) = true}
    ) -> 
        {v : tree | 
                    \(u : int), (range1:int), (range2: int). 
                (range1 == rng (ltree) /\ range2 == rng (rtree)) /\
                ((u == range1 + range2) => rng (v) == u) /\
                (tmem (v, u) = true => 
                        (u > (root -- range1) /\ 
                        root > (u -- range2))) 
                    /\ sortedtree (v) = true};    




goal : (d : {v : int | [v >0] \/ [v=0]}) -> 
        (size : {v : int | not [d > v]}) ->
        (lo : { v : int | true}) -> 
        (hi : { v : int | v == lo + d}) -> 
        {v : tree | \(u : int).
            (   hi == lo + d /\
                (tmem (v, u) = true => 
                ([u > lo] /\ [hi > u]) 
                ) /\
                sortedtree (v) = true /\
                rng (v) =  d
            )   
            };

