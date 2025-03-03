(* <!-- 7 appendN
Int -> [a] -> [a] --> 
 Has recursive structure as well. 
  *)


<!-- Finds a solution 2 sec -->
stack exec -- hplus --disable-filter=False --json='{"query": "Int -> [a] -> [a]", 
"inExamples": [],
"inArgNames": ["x", "y"]}'


<!-- Succeeds in finding a solution. 15 sec -->
stack exec -- hplus --disable-filter=False --json='{"query": "Int -> [a] -> [a]", 
"inExamples": [{ "inputs": [ "2", "[1,2,3]"], 
                "output": "[1,2,3,1,2,3]"},
                { "inputs": [ "3", "[1,2,3]"], 
                "output": "[1,2,3,1,2,3,1,2,3]"}],
"inArgNames": ["n", "xs"]}'

<!-- Fails to find  a solution -->
stack exec -- hplus --disable-filter=False --json='{"query": "Int -> [a] -> [a]", 
"inExamples": [ { "inputs": [ "1", "[1,2,3]"], 
                "output": "[1,2,3,1]"},
                { "inputs": [ "2", "[1,2,3]"], 
                "output": "[1,2,3,1,2]"},
                { "inputs": [ "3", "[1,2,3]"], 
                "output": "[1,2,3,1,2,3]"}],
"inArgNames": ["n", "xs"]}'

<!-- Refinement type -->
goal : (x : int) -> (xs : [a]) -> { v : [a] | \(u : a). mem (u, xs) = true => mem (u, v) = true /\ count (u, v) = size (v) = x};
(* 
appendsize : a :-> [a] :-> int
appendsize [] = 0 
appendsize (x :: xs) = 1

appned : x : [a] -> y : [a] -> {v : [a] | \(u : a). mem (u, x) = true => mem (u, v) /\
 mem (u, y) => mem (u, v) /\ mem (u, x) = true /\ appendsize (v) = appendsize (x) + appendsize (y)   
*)