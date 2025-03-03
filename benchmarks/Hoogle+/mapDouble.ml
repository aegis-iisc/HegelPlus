(* DoubleMap or mapIncr *)

(* Original *)
stack exec -- hplus --disable-filter=False --json='
{"query": "(a->b) -> (b -> a) -> [a]->[a]",
"inExamples": [],
"inArgNames": ["f","g","x"]}'


(* Double map application fails  *)
stack exec -- hplus --disable-filter=False --json='
{"query": "(a->b) -> (b -> a) -> [a]->[a]",
"inExamples": [{"inputs": ["\\x -> x + 1","\\y -> y + 1","[1,2,3,4]"],
              "output": "[3,4,5,6]"}],
"inArgNames": ["f","g","x"]}'





(* E4 *)
stack exec -- hplus --disable-filter=False --json='
{"query": "(a->b) -> (b -> a) -> [a]->[a]",
"inExamples": [{"inputs": ["\\x -> x + 1","\\y -> y + 1","[1,2,3,4]"],
              "output": "[3,4,5,6]"}],
"inArgNames": ["f","g","x"]}'

(*A larger query cannot be solved)*)
stack exec -- hplus --disable-filter=False --json='
{"query": "(a->b) -> (b -> a) -> [a]->[a]",
"inExamples": [],
"inArgNames": ["f","g","x"]}'

(* Refined Type *)
goal:  ??
