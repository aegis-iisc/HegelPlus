(* name : mapApply and rotate *)
(* Original Query *)
stack exec -- hplus --disable-filter=False --json='{"query": "(a->b)->[a]->[b]",
"inExamples": [],
"inArgNames": ["f","x"]}'


(* Original Monomorphic query *)
stack exec -- hplus --disable-filter=False --json='{"query": "(Int -> Int) -> [Int]->[Int]",
  "inExamples": [],
  "inArgNames": ["x"]}'



(* E2 *)
stack exec -- hplus --disable-filter=False --json='{"query": "(Int -> Int) -> [Int]->[Int]",
  "inExamples": [{ "inputs": ["\\x -> x+1", "[1,0,4,8]"],
                    "output": "[9,5,1,2]"},
                    { "inputs": ["\\x -> x", "[1,0,4,8]"],
                    "output": "[8,4,0,1]"}], 
  "inArgNames": ["f","x"]}'



(*Refinement Type *)
goal : 
